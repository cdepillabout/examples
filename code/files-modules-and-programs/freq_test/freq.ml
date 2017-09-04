
open Core

let build_counts () =
  In_channel.fold_lines In_channel.stdin ~init:Counter.empty ~f:Counter.touch

let () =
  let counts = build_counts () in
  counts
    |> Counter.to_list
    |> List.sort ~cmp:(fun (_,x) (_,y) -> Int.descending x y)
    |> (fun l -> List.take l 10)
    |> List.iter ~f:(fun (line,count) -> printf "%3d: %s\n" count line);
  match Counter.median counts with
  | Median str -> printf "median: %s\n" str
  | Before_and_after (before_str, after_str) ->
    printf "before: %s, after: %s\n" before_str after_str
