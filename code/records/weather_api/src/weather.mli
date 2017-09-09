
open Core

type weather = Sunny | Cloudy | Other
[@@deriving show]

type t =
  { raw: string;
    parsed: weather
  }
[@@deriving fields, show]

val from_string : string -> t
