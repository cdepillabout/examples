
module Scale : sig
  type t = Fahrenheit | Celcius
end

type t

val pp : Format.formatter -> t -> Ppx_deriving_runtime.unit

val create : int -> Scale.t -> t

val to_string : t -> string
