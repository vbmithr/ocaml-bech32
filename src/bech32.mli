(*---------------------------------------------------------------------------
   Copyright (c) 2018 Vincent Bernardoff. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

val convertbits :
  pad:bool -> frombits:int -> tobits:int -> string -> (string, string) result

val encode : hrp:string -> string -> (string, string) result
val encode5 : hrp:string -> string -> (string, string) result
val decode : string -> (string * string, string) result

module Segwit : sig
  module type NETWORK = sig
    type t

    val t : t
    val prefix : string
  end

  module Btc : NETWORK with type t = [`Btc]
  module Tbtc : NETWORK with type t = [`Tbtc]
  module Zil : NETWORK with type t = [`Zil]

  type 'a t = private {
    network : (module NETWORK with type t = 'a) ;
    version : int option ;
    prog : string ;
  }

  val create : ?version:int -> (module NETWORK with type t = 'a) -> string -> 'a t

  val encode : _ t -> (string, string) result
  val encode_exn : _ t -> string

  val decode : ?version:bool -> (module NETWORK with type t = 'a) -> string -> ('a t, string) result
  val decode_exn : ?version:bool -> (module NETWORK with type t = 'a) -> string -> 'a t
end

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
