open Core

module S = struct
  (* type 'a t = *)
  (*   | `True *)
  (*   | `False *)
  (*   | `Base of 'a *)

  (* type 'a lala = Lala of 'a *)

  type 'a t =
    [ `True
    | `False
    | `Base of 'a
    | `And of 'a t * 'a t
    ]
  [@@deriving show]

  let base (a : 'a) : [> `Base of 'a] = `Base a

  let true_ : [> `True] = `True

  let false_ : [> `False] = `False

  let rec and_ : 'a t list -> 'a t = function
      `True :: tl -> and_ tl
    | `False :: _ -> false_
    | other :: tl -> `And (other, and_ tl)
    | [] -> true_

  (* let rec or_ = function *)
  (*     `True :: _ -> true_ *)
  (*   | `False :: tl -> or_ tl *)
  (*   | other :: tl -> `Or (other, or_ tl) *)
  (*   | [] -> false_ *)
end

let rec eval (f : 'a -> bool) (baba : 'a S.t) : bool =
  match baba with
  | `True -> true
  | `False -> false
  | `Base a -> f a
  | `And (lala, gaga) -> eval f lala && eval f gaga
  (* | `Or (lala, gaga) -> eval f lala || eval f gaga *)

let myexpr = S.and_ [S.true_; S.base "3"]

let test = eval (fun a -> true) myexpr

(* let run () = S.show myexpr *)
let run () = print_endline @@ S.show Format.pp_print_string myexpr ;
