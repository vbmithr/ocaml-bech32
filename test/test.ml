open Astring
open Bech32

let intlen () =
  let open Pack.IntLen in
  let a = create ~len:8 0xff in
  let b = create ~len:4 0xff in
  let c = create ~len:2 0xff in
  assert (to_int a = 0xff) ;
  assert (to_int b = 0x0f) ;
  assert (to_int c = 0x03) ;
  let res = match reduce ~size:5 a with
  | None -> assert false
  | Some (v, res) ->
    assert (v = 31) ;
    assert (to_int res = 7) ;
    assert (length res = 3) ;
    res in
  match concat ~size:5 res a with
  | None, _ -> assert false
  | Some v, res ->
    assert (v = 31) ;
    assert (length res = 6 ) ;
    assert (to_int res = 63)

let compare_list ~expected l =
  let pp ppf v =
    let open Format in
    pp_print_char ppf '[' ;
    pp_print_list
      ~pp_sep:(fun ppf () -> pp_print_string ppf "; ")
      pp_print_int ppf v ;
    pp_print_char ppf ']' ;
  in
  if expected <> l then
    failwith (Format.asprintf "Got %a, expected %a@." pp l pp expected)

let pack () =
  let open Pack in
  compare_list (of_string ~size:5 "\x00\x00") ~expected:[0;0;0;0] ;
  compare_list (of_string ~size:5 "\x55\x55") ~expected:[10;21;10;16] ;
  compare_list (of_string ~size:5 "\xaa\xaa") ~expected:[21;10;21;0] ;
  compare_list (of_string ~size:5 (Hex.to_string (`Hex "6002"))) ~expected:[12; 0; 1; 0] ;
  compare_list (of_string ~size:5 (Hex.to_string (`Hex "6002751e"))) ~expected:[12; 0; 1; 7; 10; 7; 16]

let pack = [
  "intlen", `Quick, intlen ;
  "pack", `Quick, pack ;
]

let compare_string expected s =
  if s <> expected then
    failwith (Printf.sprintf "Got %s, expected %s\n" s expected)

let encode hex expected =
  let expected = String.Ascii.lowercase expected in
  let data = Hex.to_string hex in
  match encode ~hrp:"bc" data with
  | Error msg -> invalid_arg msg
  | Ok res ->
    compare_string expected res

let encode () =
  encode (`Hex "6002751e") "BC1SW50QA3JX3S"

let encode = [
  "basic", `Quick, encode ;
]

let valid_encoding s () =
  match decode s with
  | Error msg -> failwith msg
  | Ok _ -> ()

let valid_vectors = [
  "A12UEL5L";
  "a12uel5l";
  "an83characterlonghumanreadablepartthatcontainsthenumber1andtheexcludedcharactersbio1tt5tgs";
  "abcdef1qpzry9x8gf2tvdw0s3jn54khce6mua7lmqqqxw";
  "11qqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqc8247j";
  "split1checkupstagehandshakeupstreamerranterredcaperred2y9e3w";
  "?1ezyfcl"
]

let decode = ListLabels.map valid_vectors ~f:begin fun v ->
    v, `Quick, valid_encoding v
  end

let () =
  Alcotest.run "bech32" [
    "pack", pack ;
    (* "encode", encode ; *)
    "decode", decode ;
  ]

