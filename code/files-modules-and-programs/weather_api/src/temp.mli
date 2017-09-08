
module Scale : sig
  type t = Fahrenheit | Celcius
  [@@deriving show]
end

type t
[@@deriving show]

val create : int -> Scale.t -> t

val to_string : t -> string
