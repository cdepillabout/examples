open Core

type t

val show : t -> string

val pp : Format.formatter -> t -> Ppx_deriving_runtime.unit

val create : Date.t -> Temp.t -> Temp.t -> string -> t

val date : t -> Date.t
