
type t

val create : Temp.t -> string -> t

val show : t -> string

val pp : Format.formatter -> t -> Ppx_deriving_runtime.unit

val to_string : t -> string
