open Core

type t

val show : t -> string

val pp : Format.formatter -> t -> Ppx_deriving_runtime.unit

val create : Date.t -> Temp.t -> Temp.t -> string -> t

val date : t -> Date.t

val parse : Yojson.Basic.json -> t

val find : ?date:Date.t -> t List.t -> t option

val find_raw_date : string -> t List.t -> t option
