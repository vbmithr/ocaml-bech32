(*---------------------------------------------------------------------------
   Copyright (c) 2018 Vincent Bernardoff. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)
open Base
open Result.Monad_infix

let convertbits_exn ~pad ~frombits ~tobits data =
  let maxv = (1 lsl tobits) - 1 in
  let foldf =
    fun (ret, bits, acc) v ->
    let v = Char.to_int v in
    if v < 0 || v lsr frombits <> 0
    then invalid_arg ("convertbits_exn: invalid data " ^ Int.to_string v);
    let acc = (acc lsl frombits) lor v in
    let bits = bits + frombits in
    let rec loop ret bits =
      if bits >= tobits
      then (
        let bits = bits - tobits in
        loop (((acc lsr bits) land maxv) :: ret) bits)
      else ret, bits
    in
    let ret, bits = loop ret bits in
    ret, bits, acc
  in
  let ret, bits, acc = String.fold ~f:foldf ~init:([], 0, 0) data in
  let ret =
    let shift = tobits - bits in
    if pad
    then if bits <> 0 then ((acc lsl shift) land maxv) :: ret else ret
    else if (acc lsl shift) land maxv <> 0 || bits >= frombits
    then invalid_arg "convertbits_exn"
    else ret
  in
  let buf = Bytes.create (List.length ret) in
  let _ =
    List.fold_right
      ~f:(fun c i ->
        Bytes.set buf i (Char.of_int_exn c);
        Int.succ i)
      ret
      ~init:0
  in
  Bytes.unsafe_to_string ~no_mutation_while_string_reachable:buf
;;

let convertbits ~pad ~frombits ~tobits data =
  try Ok (convertbits_exn ~pad ~frombits ~tobits data) with
  | Invalid_argument msg -> Error msg
;;

let polymod_step pre =
  let open Int32 in
  let b = pre lsr 25 in
  ((pre land 0x1ffffffl) lsl 5)
  lxor (-((b lsr 0) land 1l) land 0x3b6a57b2l)
  lxor (-((b lsr 1) land 1l) land 0x26508e6dl)
  lxor (-((b lsr 2) land 1l) land 0x1ea119fal)
  lxor (-((b lsr 3) land 1l) land 0x3d4233ddl)
  lxor (-((b lsr 4) land 1l) land 0x2a1462b3l)
;;

let charset = "qpzry9x8gf2tvdw0s3jn54khce6mua7l"

let charset_rev =
  [| -1
   ; -1
   ; -1
   ; -1
   ; -1
   ; -1
   ; -1
   ; -1
   ; -1
   ; -1
   ; -1
   ; -1
   ; -1
   ; -1
   ; -1
   ; -1
   ; -1
   ; -1
   ; -1
   ; -1
   ; -1
   ; -1
   ; -1
   ; -1
   ; -1
   ; -1
   ; -1
   ; -1
   ; -1
   ; -1
   ; -1
   ; -1
   ; -1
   ; -1
   ; -1
   ; -1
   ; -1
   ; -1
   ; -1
   ; -1
   ; -1
   ; -1
   ; -1
   ; -1
   ; -1
   ; -1
   ; -1
   ; -1
   ; 15
   ; -1
   ; 10
   ; 17
   ; 21
   ; 20
   ; 26
   ; 30
   ; 7
   ; 5
   ; -1
   ; -1
   ; -1
   ; -1
   ; -1
   ; -1
   ; -1
   ; 29
   ; -1
   ; 24
   ; 13
   ; 25
   ; 9
   ; 8
   ; 23
   ; -1
   ; 18
   ; 22
   ; 31
   ; 27
   ; 19
   ; -1
   ; 1
   ; 0
   ; 3
   ; 16
   ; 11
   ; 28
   ; 12
   ; 14
   ; 6
   ; 4
   ; 2
   ; -1
   ; -1
   ; -1
   ; -1
   ; -1
   ; -1
   ; 29
   ; -1
   ; 24
   ; 13
   ; 25
   ; 9
   ; 8
   ; 23
   ; -1
   ; 18
   ; 22
   ; 31
   ; 27
   ; 19
   ; -1
   ; 1
   ; 0
   ; 3
   ; 16
   ; 11
   ; 28
   ; 12
   ; 14
   ; 6
   ; 4
   ; 2
   ; -1
   ; -1
   ; -1
   ; -1
   ; -1
  |]
;;

let invalid_hrp c = Char.(c < '\033' || c > '\126' || (c >= 'A' && c <= 'Z'))

let check_hrp chk i hrp =
  try
    let chk, i =
      String.fold
        ~f:(fun (a, i) c ->
          if invalid_hrp c then raise Stdlib.Exit;
          let c = Char.to_int c |> Int32.of_int_exn in
          Int32.(polymod_step a lxor (c lsr 5)), Int.succ i)
        ~init:(chk, i)
        hrp
    in
    Ok (chk, i)
  with
  | Stdlib.Exit -> Error "invalid hrp"
;;

let encode5 ~hrp data =
  let datalen = String.length data in
  check_hrp 1l 0 hrp
  >>= fun (chk, i) ->
  (if i + 7 + datalen > 90 then Error "data too long" else Ok ())
  >>= fun () ->
  let chk = polymod_step chk in
  let buf = Bytes.create String.(length hrp + 1 + datalen + 6) in
  let chk, i =
    String.fold
      ~f:(fun (a, i) c ->
        let open Int32 in
        Bytes.set buf i c;
        let a = polymod_step a lxor (Int.(Char.to_int c land 0x1f) |> of_int_exn) in
        a, Int.succ i)
      ~init:(chk, 0)
      hrp
  in
  Bytes.set buf i '1';
  let chk, i =
    String.fold
      ~f:(fun (a, i) c ->
        let c = Char.to_int c in
        Bytes.set buf i charset.[c];
        let open Int32 in
        polymod_step a lxor of_int_exn c, Int.succ i)
      ~init:(chk, Int.succ i)
      data
  in
  let chk =
    List.fold_left ~f:(fun a () -> polymod_step a) ~init:chk [ (); (); (); (); (); () ]
  in
  let chk = Int32.(chk lxor 1l) in
  let compute_chk chk i =
    let shift = (5 - i) * 5 in
    Int32.((chk lsr shift) land 0x1fl |> to_int_exn)
  in
  for j = 0 to 5 do
    Bytes.set buf (i + j) charset.[compute_chk chk j]
  done;
  Ok (Bytes.unsafe_to_string ~no_mutation_while_string_reachable:buf)
;;

let encode ~hrp data = convertbits ~pad:true ~frombits:8 ~tobits:5 data >>= encode5 ~hrp

let decode bech32 =
  let len = String.length bech32 in
  (if len < 8 || len > 90 then Error "bad input length" else Ok ())
  >>= fun () ->
  Result.of_option (String.rsplit2 bech32 ~on:'1') ~error:"no 1 found in address"
  >>= fun (hrp, payload) ->
  let hrplen = String.length hrp in
  let payloadlen = len - 1 - hrplen in
  (if hrplen < 1 || payloadlen < 6 then Error "bad hrp/payload length" else Ok ())
  >>= fun () ->
  let have_upper = ref false in
  let have_lower = ref false in
  (try
     String.fold
       ~f:(fun a c ->
         let cint = Char.(to_int (lowercase c)) in
         if cint < 33 || cint > 126 then raise Stdlib.Exit;
         if Char.is_uppercase c then have_upper := true;
         if Char.is_lowercase c then have_lower := true;
         Int32.(polymod_step a lxor (Int.(cint lsr 5) |> Int32.of_int_exn)))
       ~init:1l
       hrp
     |> Result.return
   with
   | Stdlib.Exit -> Error ("invalid hrp char in " ^ hrp))
  >>= fun chk ->
  let chk = polymod_step chk in
  let chk =
    String.fold
      ~f:(fun a c ->
        let open Int32 in
        polymod_step a lxor (Int.(Char.to_int c land 0x1f) |> Int32.of_int_exn))
      ~init:chk
      hrp
  in
  let buf = Bytes.create (payloadlen - 6) in
  (try
     String.fold
       ~f:(fun (a, i) c ->
         if Char.is_uppercase c then have_upper := true;
         if Char.is_lowercase c then have_lower := true;
         let ci = Char.to_int c in
         if ci land 0x80 <> 0 then raise Stdlib.Exit;
         let v = charset_rev.(ci) in
         if i + 6 < payloadlen then Bytes.set buf i (Char.of_int_exn v);
         Int32.(polymod_step a lxor of_int_exn v), Int.succ i)
       ~init:(chk, 0)
       payload
     |> fst
     |> Result.return
   with
   | _ -> Error "invalid data")
  >>= fun chk ->
  if Bool.(!have_upper <> !have_lower && Int32.(chk = 1l))
  then Ok (hrp, Bytes.unsafe_to_string ~no_mutation_while_string_reachable:buf)
  else Error "wrong chksum or mixed case"
;;

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
