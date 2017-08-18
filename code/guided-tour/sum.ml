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

let mypoint = { x = 3.; y = 5.5 }

let fff { x; y } = x +. y

type circle_desc  = { center: point2d; radius: float }
type rect_desc    = { lower_left: point2d; width: float; height: float }
type segment_desc = { endpoint1: point2d; endpoint2: point2d }

let magnitude { x; y } = sqrt (x ** 2. +. y ** 2.)

let distance v1 v2 =
  magnitude { x = v1.x -. v2.x; y = v1.y -. v2.y }

type scene_element =
  | Circle  of { center: point2d; radius: float }
  | Rect    of rect_desc
  | Segment of segment_desc

let is_inside_scene_element point scene_element =
  match scene_element with
  | Circle { center; radius } ->
    distance center point < radius
  | Rect { lower_left; width; height } ->
    point.x    > lower_left.x && point.x < lower_left.x +. width
    && point.y > lower_left.y && point.y < lower_left.y +. height
  | Segment { endpoint1; endpoint2 } -> false

let is_inside_scene point scene =
     List.exists scene
       ~f:(fun el -> is_inside_scene_element point el)

let scene_test = is_inside_scene mypoint [Circle {center = mypoint; radius = 3.}]

let arraytest =
  let nums = [| 1; 2; 3; 4 |] in
  nums.(2) <- 100 ;
  nums

type running_sum =
  { mutable sum: float;
    mutable sum_sq: float;
    mutable samples: int;
  }

let mean {sum; samples} = sum /. Float.of_int samples

let stdev rsum =
  sqrt (rsum.sum_sq /. float rsum.samples -.
        (rsum.sum /. float rsum.samples) ** 2.)

let create_rsum =
  { sum = 0.; sum_sq = 0. ; samples = 0 }

let update_rsum rsum x =
  rsum.samples <- rsum.samples + 1;
  rsum.sum <- rsum.sum +. x;
  rsum.sum_sq <- rsum.sum_sq +. x *. x

let () =
  List.iter [1.;3.;2.;-7.;4.;5.] ~f:(update_rsum create_rsum)

let dorsum = create_rsum

let tryref =
  let x = ref 10 in
  let y = !x in
  x := !x + 1;
  !x + y

let main () =
  printf "Total: %F\n" (read_and_accumulate 0.)
