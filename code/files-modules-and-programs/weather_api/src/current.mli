
type t
[@@deriving show]

val create : Temp.t -> string -> t

val to_string : t -> string

val parse : Yojson.Basic.json -> t
