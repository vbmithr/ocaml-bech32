(*---------------------------------------------------------------------------
   Copyright (c) 2018 Vincent Bernardoff. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)
open Rresult
open Astring

module Pack = struct
  module IntLen = struct
    let mask v = function
      | 0 -> 0
      | 1 -> v land 0x01
      | 2 -> v land 0x03
      | 3 -> v land 0x07
      | 4 -> v land 0x0f
      | 5 -> v land 0x1f
      | 6 -> v land 0x3f
      | 7 -> v land 0x7f
      | 8 -> v
      | _ -> invalid_arg "Packer.IntLen.mask"

    type t = {
      v : int ;
      len : int ;
    }

    let length { len } = len

    let empty = { v = 0 ; len = 0 }
    let create ~len v = { v ; len }

    let to_int { v ; len } = mask v len
    let (<<) t i = to_int t lsl i
    let (>>) t i = to_int t lsr i

    let reduce ~size ({ v ; len } as t) =
      match len - size with
      | newlen when newlen < 0 -> None
      | newlen -> Some (t >> newlen, { v ; len = newlen })

    let concat ~size ({ v ; len } as t) ({ v = v2 ; len = len2 } as t2) =
      if len + len2 < size then
        None,
        { v = (t << len2) lor (to_int t2) ; len = len + len2 }
      else
        let needed = size - len in
        let unneeded = len2 - needed in
        Some ((t << needed) lor v2 lsr unneeded),
        { v = v2 ; len = unneeded }
  end

  type t = {
    size : int ;
    acc : int list ;
    p : IntLen.t ;
  }

  let create ~size =
    if size < 1 || size > 7 then
      invalid_arg "Packer.create" ;
    { size ; acc = [] ; p = IntLen.empty }

  let rec feed ({ size ; acc ; p } as t) v =
    let v = IntLen.create ~len:8 v in
    match IntLen.concat ~size p v with
    | None, p' -> { t with p = p' }
    | Some e, p' ->
      match IntLen.reduce ~size p' with
      | None -> { t with acc = e :: acc ; p = p' }
      | Some (e', p') -> { t with acc = e' :: e :: acc ; p = p' }

  let feed_char t c = feed t (Char.to_int c)

  let finalize { size ; acc ; p } =
    List.rev IntLen.((p << (size - p.len)) :: acc)

  let of_string ~size s =
    String.fold_left feed_char (create ~size) s |> finalize
end

module Int32 = struct
  include Int32
  external (~-) : t -> t = "%int32_neg"
  external (+) : t -> t -> t = "%int32_add"
  external (-) : t -> t -> t = "%int32_sub"
  external (lsl) : t -> int -> t = "%int32_lsl"
  external (lsr) : t -> int -> t = "%int32_lsr"
  external (land) : t -> t -> t = "%int32_and"
  external (lor) : t -> t -> t = "%int32_or"
  external (lxor) : t -> t -> t = "%int32_xor"

  let of_char c = c |> Char.to_int |> of_int
end

(* let generator = [0x3b6a57b2l; 0x26508e6dl; 0x1ea119fal; 0x3d4233ddl; 0x2a1462b3l] *)
(* let polymod vs =
 *   ListLabels.fold_left vs ~init:1l ~f:begin fun chk v ->
 *     let top = Int32.(chk lsr 25) in
 *     let chk = Int32.(((chk land 0x1fff_ffffl) lsl 5) lxor v) in
 *     ListLabels.fold_left generator ~init:(chk, 0) ~f:begin fun (chk, i) g ->
 *         Int32.logxor chk (if Int32.((top lsr i) land 1l) <> 0l then g else 0l), succ i
 *       end |> fst
 *   end *)

let polymod_step pre =
  let open Int32 in
  let b = pre lsr 25 in
  ((pre land 0x1ffffffl) lsl 5) lxor
  (-((b lsr 0) land 1l) land 0x3b6a57b2l) lxor
  (-((b lsr 1) land 1l) land 0x26508e6dl) lxor
  (-((b lsr 2) land 1l) land 0x1ea119fal) lxor
  (-((b lsr 3) land 1l) land 0x3d4233ddl) lxor
  (-((b lsr 4) land 1l) land 0x2a1462b3l)

let charset = "qpzry9x8gf2tvdw0s3jn54khce6mua7l"
let charset_rev = [|
  -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1;
  -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1;
  -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1;
  15; -1; 10; 17; 21; 20; 26; 30;  7;  5; -1; -1; -1; -1; -1; -1;
  -1; 29; -1; 24; 13; 25;  9;  8; 23; -1; 18; 22; 31; 27; 19; -1;
   1;  0;  3; 16; 11; 28; 12; 14;  6;  4;  2; -1; -1; -1; -1; -1;
  -1; 29; -1; 24; 13; 25;  9;  8; 23; -1; 18; 22; 31; 27; 19; -1;
   1;  0;  3; 16; 11; 28; 12; 14;  6;  4;  2; -1; -1; -1; -1; -1
|]

let invalid_hrp c =
  c < '\033' || c > '\126' || (c >= 'A' && c <= 'Z')

let check_hrp chk i hrp =
  try
    let chk, i =
      String.fold_left begin fun (a, i) c ->
        if invalid_hrp c then raise Exit ;
        Int32.(polymod_step a lxor (of_char c lsr 5)), succ i
      end (chk, i) hrp in
    Ok (chk, i)
  with Exit -> Error "invalid hrp"

let encode ~hrp data =
  let data = Pack.of_string ~size:5 data in
  let datalen = List.length data in
  check_hrp 1l 0 hrp >>= fun (chk, i) ->
  (if i + 7 + datalen > 90 then
     Error "data too long" else Ok ()) >>= fun () ->
  let chk = polymod_step chk in
  let buf = Bytes.create String.(length hrp + 1 + datalen + 6) in
  let chk, i = String.fold_left (fun (a, i) c ->
      Bytes.set buf i c ;
      Int32.logxor (polymod_step a) (Char.to_int c land 0x1f |> Int32.of_int),
      succ i) (chk, 0) hrp in
  Bytes.set buf i '1' ;
  let chk, i =
    List.fold_left (fun (a, i) d ->
        Bytes.set buf i (String.get charset d) ;
        Int32.logxor (polymod_step a) (Int32.of_int d), succ i)
      (chk, succ i) data in
  let chk = List.fold_left
      (fun a () -> polymod_step a) chk [();();();();();()] in
  let chk = Int32.(chk lxor 1l) in
  let compute_chk chk i =
    let shift = (5 - i) * 5 in
    Int32.((chk lsr shift) land 0x1fl |> to_int) in
  for i = i to i+5 do
    Bytes.set buf i (String.get charset (compute_chk chk i))
  done ;
  Ok (Bytes.unsafe_to_string buf)

let decode bech32 =
  let len = String.length bech32 in
  (if len < 8 || len > 90 then
     Error "bad input length" else Ok ()) >>= fun () ->
  match String.cut bech32 ~sep:"1" with
  | None -> Error "missing separator"
  | Some (hrp, payload) ->
    let hrplen = String.length hrp in
    let payloadlen = len - 1 - hrplen in
    (if hrplen < 1 || payloadlen < 6 then
       Error "bad hrp/payload length" else Ok ()) >>= fun () ->
    let have_upper = ref false in
    let have_lower = ref false in
    let chk = String.fold_left begin fun a c ->
        if Char.Ascii.is_upper c then have_upper := true ;
        if Char.Ascii.is_lower c then have_lower := true ;
        Int32.logxor (polymod_step a)
          (Char.(to_int (Ascii.lowercase c)) lsr 5 |> Int32.of_int)
      end 1l hrp in
    let chk = polymod_step chk in
    let chk = String.fold_left begin fun a c ->
        Int32.logxor (polymod_step a) (Char.to_int c land 0x1f |> Int32.of_int)
      end chk hrp in
    let buf = Bytes.create (payloadlen - 6) in
    begin try
        String.fold_left begin fun (a, i) c ->
          if Char.Ascii.is_upper c then have_upper := true ;
          if Char.Ascii.is_lower c then have_lower := true ;
          let ci = Char.to_int c in
          if ci land 0x80 <> 0 then raise Exit ;
          let v = charset_rev.(ci) in
          if i + 6 < payloadlen then
            Bytes.set buf i (Char.of_byte v) ;
          Int32.logxor (polymod_step a) (Int32.of_int v),
          succ i
        end (chk, 0) payload |> fst |> R.ok
      with Exit -> Error "invalid data"
    end >>= fun chk ->
    if !have_upper <> !have_lower && chk = 1l then
      Ok (hrp, Bytes.unsafe_to_string buf)
    else Error "wrong chksum or mixed case"

(*---------------------------------------------------------------------------
   Copyright (c) 2018 Vincent Bernardoff

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
