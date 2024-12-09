module type NETWORK = sig
  val versioned : bool
  val prefix : string
end

module type S = sig
  type t

  val version : t -> int option
  val prog : t -> string
  val scriptPubKey : t -> string
  val pp : Format.formatter -> t -> unit
  val create : ?version:int -> string -> t
  val to_string : t -> string
  val of_string : string -> t
  val of_string_error : string -> (t, string) result
end
