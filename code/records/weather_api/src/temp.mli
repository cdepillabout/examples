
module Scale : sig
  type t = Fahrenheit | Celcius
  [@@deriving show]
end

type t
[@@deriving show]

val create : temp:int -> scale:Scale.t -> t

val to_string : t -> string

val succ_temp : t -> t
