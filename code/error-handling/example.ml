
open Core

exception Key_not_found of string [@@deriving sexp]

let rec find_exn alist key = match alist with
    | [] -> raise (Key_not_found key)
    | (key',data) :: tl -> if key = key' then data else find_exn tl key

let alist = [("a",1); ("b",2)]

let find_safe list key =
  try Ok (find_exn list key) with
  | Key_not_found str -> Error str

let find_safe' list key =
  printf "before fun...\n" ;
  let res =
    Core.Result.try_with
      (fun _ ->
         printf "before (in) fun...\n" ;
         let res = find_exn list key in
         printf "after (in) fun...\n" ;
         res
      )
  in
  printf "after fun...\n" ;
  res

let find_safe'' list key =
  Or_error.try_with (fun _ -> find_exn list key)

let find_safe''' alist key =
    Option.try_with (fun _ -> find_exn alist key)

let run () =
  (* printf "%d\n" @@ find_exn alist "a" ; *)
  (try printf "found (find_ex): %d\n" @@ find_exn alist "c" with
  | Key_not_found str -> printf "Couldn't find value for key (find_ex): %s\n" str
  );
  (match find_exn alist "c" with
  | exception (Key_not_found str) ->
    printf "Couldn't find value for key (find_ex version 2): %s\n" str
  | v -> printf "found (find_ex version 2): %d\n" v
  );
  (match find_safe alist "c" with
  | Ok res -> printf "found (find_safe): %d\n" res
  | Error err -> printf "not found (find_safe): %s\n" err
  );
  printf "hello\n";
  (match find_safe' alist "c" with
  | Ok res -> printf "found (find_safe'): %d\n" res
  | Error (Key_not_found str) ->
    printf "lalala\n" ;
    printf "not found (find_safe'): %s\n" str ;
    printf "lalala2\n"
  | Error _ -> printf "found other exception...\n"
  );
  printf "hello2\n";
  (match find_safe'' alist "c" with
  | Ok res -> printf "found (find_safe''): %d\n" res
  | Error err -> printf "got error (find_safe''): %s\n" (Error.to_string_hum err);
  );
  (match find_safe''' alist "c" with
  | Some res -> printf "found (find_safe'''): %d\n" res
  | None -> printf "not found (find_safe''')\n";
  )

let lookup_weight ~compute_weight alist key =
  try
    let data = List.Assoc.find_exn alist key in
    compute_weight data
  with
    Not_found -> 0.

(* If compute_weight throws an exception in the previous lookup_weight, then it
   will be incorrectly caught. Writing the code the following way stops
   lookup_weight from incorrect catching errors thrown by compute_weight.
*)
let lookup_weight' ~compute_weight alist key =
  match
    try Some (List.Assoc.find_exn alist key)
    with _ -> None
  with
  | None -> 0.
  | Some data -> compute_weight data

(* OCaml allows for exceptions to be caught by match statements directly. *)
let lookup_weight ~compute_weight alist key =
  match List.Assoc.find_exn alist key with
  | exception _ -> 0.
  | data -> compute_weight data

(* Turn exception into an Or_error (which is a Result with the error being Error *)
let find alist key =
    Or_error.try_with (fun () -> find_exn alist key)

(* Turn Or_error back into exception. *)
let () =
  let int = Or_error.ok_exn (find ["a",1; "b",2] "b") in
  printf "Res: %d\n" int

let () = run ()
