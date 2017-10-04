open Core

type basic_color =
  [ `Black   | `Blue | `Cyan  | `Green
  | `Magenta | `Red  | `White | `Yellow ]

type weight = [ `Bold | `Regular ]

type color =
  [ `Basic of basic_color * weight
  | `Gray of int
  | `RGB  of int * int * int ]

type extended_color =
  [ color
  | `RGBA of int * int * int * int ]

val color_to_int          : color -> int
val extended_color_to_int : extended_color -> int
val extended_color_to_int' : extended_color -> int
val extended_color_to_int'' : extended_color -> int
