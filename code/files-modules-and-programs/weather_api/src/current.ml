open Core

type t =
  { temp: Temp.t;
    weather: string
  }
[@@deriving show]

let create temp weather = {temp; weather}

let to_string {temp; weather} =
  Temp.to_string temp ^ " " ^ weather
