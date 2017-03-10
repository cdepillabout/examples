open Core.Std

let build_counts () =
  In_channel.fold_lines stdin ~init:Counter.empty ~f:Counter.touch

let print_top counter =
  Counter.to_list counter
  |> List.sort ~cmp:(fun (_,x) (_,y) -> Int.descending x y)
  |> (fun counts -> List.take counts 10)
  |> List.iter ~f:(fun (line,count) -> printf "%3d: %s\n" count line)

let print_median counter =
  Use_median_1.print_median (Counter.median counter)

let () =
  let counter = build_counts () in
  print_top counter ;
  (* TODO: Ask question on stack overflow about how to find functions based on
   * names or type signatures. *)
  printf "\n" ;
  print_median counter
