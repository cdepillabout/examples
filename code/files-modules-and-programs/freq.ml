open Core

let build_counts () : (string, int) List.Assoc.t =
  In_channel.fold_lines
    In_channel.stdin
    ~init:[]
    ~f:(fun (counts : (string, int) List.Assoc.t) (line : string) ->
        let count =
          match List.Assoc.find counts line ~equal:String.equal with
          | None -> 0
          | Some x -> x
        in
        List.Assoc.add ~equal:String.equal counts line (count + 1)
      )

let run () =
  build_counts ()
  |> List.sort ~cmp:(fun (_,(x : int)) (_,(y : int)) -> Int.descending x y)
  |> (fun l -> List.take l 10)
  |> List.iter ~f:(fun (line,count) -> printf "%3d: %s\n" count line)

let () = run ()
