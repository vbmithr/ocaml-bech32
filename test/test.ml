open Astring
open Bech32
open Alcotest

let compare_string expected s =
  if s <> expected then
    failwith (Printf.sprintf "Got %s, expected %s\n" s expected)

let compare_bytes expected s =
  let expected = Hex.of_string expected in
  let s = Hex.of_string s in
  if s <> expected then
    failwith (Format.asprintf "Got %a, expected %a\n" Hex.pp s Hex.pp expected)

let test_changebase_simple () =
  let buf = Bigstring.create 100 in
  for _ = 0 to 100 do
    let _ = Monocypher.Rand.write buf in
    let rand = Bigstring.to_string buf in
    match convertbits ~pad:true ~frombits:8 ~tobits:5 rand with
    | Error msg ->
      failwith (Printf.sprintf "from %d to %d: %s" 8 5 msg)
    | Ok rand' ->
      match convertbits ~pad:false ~frombits:5 ~tobits:8 rand' with
      | Error msg ->
        failwith (Printf.sprintf "from %d to %d: %s" 5 8 msg)
      | Ok rand'' -> compare_bytes rand rand''
  done

(* let test_changebase () =
 *   let buf = Bigstring.create 100 in
 *   for _ = 0 to 100 do
 *     for frombits = 5 to 8 do
 *       for tobits = 5 to 8 do
 *         let _ = Monocypher.Rand.write buf in
 *         let rand = Bigstring.to_string buf in
 *         match convertbits ~pad:true ~frombits:8 ~tobits:frombits rand with
 *         | Error msg ->
 *           failwith (Printf.sprintf "from %d to %d: %s" frombits tobits msg)
 *         | Ok rand' ->
 *           match convertbits ~pad:true ~frombits ~tobits rand' with
 *           | Error msg ->
 *             failwith (Printf.sprintf "from %d to %d: %s" frombits tobits msg)
 *           | Ok rand'' ->
 *             match convertbits ~pad:false ~frombits:tobits ~tobits:frombits rand' with
 *             | Error msg ->
 *               failwith (Printf.sprintf "from %d to %d: %s" tobits frombits msg)
 *             | Ok rand''' -> compare_bytes rand rand''
 *       done
 *     done
 *   done *)

let test_changebase = [
  "random_simple", `Quick, test_changebase_simple ;
]

let valid_vectors = [
  "0014751e76e8199196d454941c45d1b3a323f1433bd6", "BC1QW508D6QEJXTDG4Y5R3ZARVARY0C5XW7KV8F3T4" ;
  "00201863143c14c5166804bd19203356da136c985678cd4d27a1b8c6329604903262", "tb1qrp33g0q5c5txsp9arysrx4k6zdkfs4nce4xj0gdcccefvpysxf3q0sl5k7" ;
  "6002751e", "BC1SW50QA3JX3S" ;
  "5210751e76e8199196d454941c45d1b3a323", "bc1zw508d6qejxtdg4y5r3zarvaryvg6kdaj" ;
  "0020000000c4a5cad46221b2a187905e5266362b99d5e91c6ce24d165dab93e86433", "tb1qqqqqp399et2xygdj5xreqhjjvcmzhxw4aywxecjdzew6hylgvsesrxh6hy" ;
]

let segwit_scriptpubkey ?(version=0) witprog =
  let proglen = String.length witprog in
  let buf = Bytes.create (proglen + 2) in
  Bytes.set buf 0
    (Char.of_byte ((match version with 0 -> 0 | v -> 0x50 + v))) ;
  Bytes.set buf 1 (Char.of_byte proglen) ;
  Bytes.blit_string witprog 0 buf 2 proglen ;
  Bytes.unsafe_to_string buf

let decode_check_valid hex v () =
  match Segwit.decode v with
  | Error msg -> failwith msg
  | Ok ({ network = _ ; version ; prog } as t) ->
    match version with
    | None -> fail "version"
    | Some version ->
      let script = segwit_scriptpubkey ~version prog in
      let `Hex decoded = Hex.of_string script in
      compare_string hex decoded ;
      match Segwit.encode t with
      | Error msg -> failwith msg
      | Ok addr -> compare_string (String.Ascii.lowercase v) addr

let decode_check_valid = ListLabels.map valid_vectors ~f:begin fun (hex, v) ->
    v, `Quick, decode_check_valid hex v
  end

let invalid_vectors = [
  "tc1qw508d6qejxtdg4y5r3zarvary0c5xw7kg3g4ty" ;
  "bc1qw508d6qejxtdg4y5r3zarvary0c5xw7kv8f3t5" ;
  "BC13W508D6QEJXTDG4Y5R3ZARVARY0C5XW7KN40WF2" ;
  "bc1rw5uspcuh" ;
  "bc10w508d6qejxtdg4y5r3zarvary0c5xw7kw508d6qejxtdg4y5r3zarvary0c5xw7kw5rljs90" ;
  "BC1QR508D6QEJXTDG4Y5R3ZARVARYV98GJ9P" ;
  "tb1qrp33g0q5c5txsp9arysrx4k6zdkfs4nce4xj0gdcccefvpysxf3q0sL5k7" ;
  "bc1zw508d6qejxtdg4y5r3zarvaryvqyzf3du" ;
  "tb1qrp33g0q5c5txsp9arysrx4k6zdkfs4nce4xj0gdcccefvpysxf3pjxtptv" ;
  "bc1gmk9yu" ;
]

let decode_invalid v () =
  match Segwit.decode v with
  | Error _ -> ()
  | Ok { network = _ ; version = _ ; prog } ->
    failwith (Printf.sprintf "prog=[%S]" prog)

let decode_check_invalid = ListLabels.map invalid_vectors ~f:begin fun v ->
    v, `Quick, decode_invalid v
  end

let valid_vectors = [
  "A12UEL5L";
  "a12uel5l";
  "an83characterlonghumanreadablepartthatcontainsthenumber1andtheexcludedcharactersbio1tt5tgs";
  "abcdef1qpzry9x8gf2tvdw0s3jn54khce6mua7lmqqqxw";
  "11qqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqc8247j";
  "split1checkupstagehandshakeupstreamerranterredcaperred2y9e3w";
  "?1ezyfcl"
]

let invalid_vectors = [
  "\x201nwldj5";
  "\x7f1axkwrx";
  "\x801eym55h";
  "an84characterslonghumanreadablepartthatcontainsthenumber1andtheexcludedcharactersbio1569pvx";
  "pzry9x0s0muk";
  "1pzry9x0s0muk";
  "x1b4n0q5v";
  "li1dgmt3";
  "de1lg7wt\xff";
  "A1G7SGD8";
  "10a06t8";
  "1qzzfhee";
]

let valid_encoding s () =
  match decode s with
  | Error msg -> failwith msg
  | Ok _ -> ()

let invalid_encoding s () =
  match decode s with
  | Error _ -> ()
  | Ok _ -> failwith ""

let decode_valid =
  ListLabels.map valid_vectors ~f:begin fun v ->
    v, `Quick, valid_encoding v
  end

let decode_invalid =
  ListLabels.map invalid_vectors ~f:begin fun v ->
    v, `Quick, invalid_encoding v
  end

let () =
  Alcotest.run "bech32" [
    "changebase", test_changebase ;
    "segwit_decode_valid", decode_check_valid ;
    "segwit_decode_invalid", decode_check_invalid ;
    "decode_valid", decode_valid ;
    "decode_invalid", decode_invalid ;
  ]

