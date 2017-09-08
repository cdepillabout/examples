
type t
[@@deriving show]

val create : temp:Temp.t -> weather:string -> t

val to_string : t -> string

val parse : Yojson.Basic.json -> t
