open Core

module Scale = struct
  type t = Fahrenheit | Celcius
  [@@deriving show]
end

type t =
  { temp: int;
    scale: Scale.t
  }
[@@deriving show]

let create temp scale = {temp; scale}

let to_string {temp; scale} =
  match scale with
  | Fahrenheit -> string_of_int temp ^ "F"
  | Celcius -> string_of_int temp ^ "C"
