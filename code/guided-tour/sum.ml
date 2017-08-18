open Core

let rec read_and_accumulate (accum : float) : float =
  let line = In_channel.input_line In_channel.stdin in
  match line with
  | None -> accum
  | Some x -> read_and_accumulate (accum +. Float.of_string x)

let what = 1,2,3

let who = [1;2;3;] @ [4;5;6]

let lala =
  let mywhat = 7 in
  let whowho = 10 in
  mywhat + whowho

type point2d =
  { x : float
  ; y : float
  }

let mypoint = {x = 3.; y = 5.5}

let fff {x; y} = x +. y

let main () =
  printf "Total: %F\n" (read_and_accumulate 0.)
