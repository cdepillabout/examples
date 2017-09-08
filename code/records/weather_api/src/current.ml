open Core

type t =
  { temp: Temp.t;
    weather: string
  }
[@@deriving show]

let create temp weather = {temp; weather}

let to_string {temp; weather} =
  Temp.to_string temp ^ " " ^ weather

let parse item_json =
  let open Yojson.Basic.Util in
  let condition = item_json |> member "condition" in
  let temp_int = condition |> member "temp" |> to_string |> Int.of_string in
  let weather = condition |> member "text" |> to_string in
  let temp = Temp.create temp_int Temp.Scale.Celcius in
  create temp weather

