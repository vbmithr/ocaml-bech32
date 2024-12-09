open Base
open Result.Monad_infix
open Common
include Segwit_intf

module Btc = struct
  let versioned = true
  let prefix = "bc"
end

module Tbtc = struct
  let versioned = true
  let prefix = "tb"
end

module Zil = struct
  let versioned = false
  let prefix = "zil"
end

module Make (N : NETWORK) = struct
  type t =
    { version : int option
    ; prog : string
    }

  let version { version; _ } = version
  let prog { prog; _ } = prog

  let scriptPubKey { prog; version; _ } =
    match version with
    | None -> invalid_arg "scriptPubKey: version"
    | Some v when v < 0 || v > 16 -> invalid_arg "scriptPubKey: version"
    | Some version ->
      let proglen = String.length prog in
      let buf = Bytes.create (proglen + 2) in
      Bytes.set
        buf
        0
        (Char.of_int_exn
           (match version with
            | 0 -> 0
            | n -> 0x50 + n));
      Bytes.set buf 1 (Char.of_int_exn proglen);
      Bytes.From_string.blit ~src:prog ~src_pos:0 ~dst:buf ~dst_pos:2 ~len:proglen;
      Bytes.unsafe_to_string ~no_mutation_while_string_reachable:buf
  ;;

  let check_version v = if v < 0 || v > 16 then invalid_arg "invalid Segwit version"

  let create ?version prog =
    Option.iter ~f:check_version version;
    { version; prog }
  ;;

  let to_string { version; prog } =
    let prog = convertbits_exn ~pad:true ~frombits:8 ~tobits:5 prog in
    let proglen = String.length prog in
    match version with
    | None ->
      let buf = Bytes.create proglen in
      Bytes.From_string.blito ~src:prog ~dst:buf ();
      encode5
        ~hrp:N.prefix
        (Bytes.unsafe_to_string ~no_mutation_while_string_reachable:buf)
    | Some version ->
      let buf = Bytes.create (proglen + 1) in
      Bytes.set buf 0 (Char.of_int_exn version);
      Bytes.From_string.blito ~src:prog ~dst:buf ~dst_pos:1 ();
      encode5
        ~hrp:N.prefix
        (Bytes.unsafe_to_string ~no_mutation_while_string_reachable:buf)
  ;;

  let to_string t =
    match to_string t with
    | Error _ -> assert false
    | Ok v -> v
  ;;

  let pp ppf t = Stdlib.Format.pp_print_string ppf (to_string t)

  let of_string_error addr =
    decode addr
    >>= fun (hrp, data) ->
    let datalen = String.length data in
    (if datalen < 5 then Error "invalid segwit data" else Ok ())
    >>= fun () ->
    let hrplow = String.lowercase hrp in
    if not (String.equal hrplow N.prefix)
    then Result.failf "invalid segwit hrp: expected %s actual %s" N.prefix hrplow
    else
      Ok ()
      >>= fun () ->
      match N.versioned with
      | false ->
        convertbits data ~pad:false ~frombits:5 ~tobits:8
        >>= fun decoded ->
        let decodedlen = String.length decoded in
        (if decodedlen = 0 || decodedlen < 2 || decodedlen > 40
         then Error "invalid segwit data"
         else Ok ())
        >>= fun () ->
        (if decodedlen <> 20 && decodedlen <> 32
         then Error "invalid segwit length"
         else Ok ())
        >>= fun () -> Ok (create decoded)
      | true ->
        let decoded = String.sub data ~pos:1 ~len:(datalen - 1) in
        convertbits decoded ~pad:false ~frombits:5 ~tobits:8
        >>= fun decoded ->
        let decodedlen = String.length decoded in
        (if decodedlen = 0 || decodedlen < 2 || decodedlen > 40
         then Error "invalid segwit data"
         else Ok ())
        >>= fun () ->
        let version = data.[0] |> Char.to_int in
        (if version > 16 then Error "invalid segwit version" else Ok ())
        >>= fun () ->
        (if version = 0 && decodedlen <> 20 && decodedlen <> 32
         then Error "invalid segwit length"
         else Ok ())
        >>= fun () -> Ok (create ~version decoded)
  ;;

  let of_string t =
    match of_string_error t with
    | Error e -> invalid_arg e
    | Ok v -> v
  ;;
end
