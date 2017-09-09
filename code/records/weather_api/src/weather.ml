
open Core

type weather = Sunny | Cloudy | Other
[@@deriving show]

type t =
  { raw: string;
    parsed: weather
  }
[@@deriving fields, show]

let from_string raw =
  let opts = [Re_posix.(`ICase)] in
  let is_sunny = Re.execp (Re_posix.compile_pat ~opts "sunny") raw in
  let is_cloudy = Re.execp (Re_posix.compile_pat ~opts "cloudy") raw in
  let parsed =
    match is_sunny, is_cloudy with
    | true, _ -> Sunny
    | _, true -> Cloudy
    | _ -> Other
  in
  { raw; parsed}
