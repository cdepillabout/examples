open Core

let rec destutter = function
  | [] as l -> l
  | hd :: (hd' :: tl as l) when hd = hd' -> destutter l
  | hd :: tl -> hd :: destutter tl

let rec destutter_tc accum = function
  | [] -> accum
  | hd :: (hd' :: tl as l) when hd = hd' -> destutter_tc accum l
  | hd :: tl -> destutter_tc (hd :: accum) tl

let destutter' = Fn.compose List.rev (destutter_tc [])

let rec remove_evens = function
  | [] as l -> l
  | num :: tl when num mod 2 = 0 -> remove_evens tl
  | hd :: tl -> hd :: remove_evens tl
