
open Core

(***********)
(** Color **)
(***********)

type basic_color =
  Black | Red | Green | Yellow | Blue | Magenta | Cyan | White

let basic_color_to_int = function
  | Black -> 0
  | Red -> 1
  | Green -> 2
  | Yellow -> 3
  | Blue -> 4
  | Magenta -> 5
  | Cyan -> 6
  | White -> 7

let color_by_number number text =
      sprintf "\027[38;5;%dm%s\027[0m" number text

type weight = Regular | Bold

type color =
  | Basic of basic_color * weight (* basic colors, regular and bold *)
  | RGB   of int * int * int      (* 6x6x6 color cube *)
  | Gray  of int                  (* 24 grayscale levels *)

let color_to_int = function
  | Basic (basic_color,weight) ->
    let base = match weight with Bold -> 8 | Regular -> 0 in
    base + basic_color_to_int basic_color
  | RGB (r,g,b) -> 16 + b + g * 6 + r * 36
  | Gray i -> 232 + i

let color_print color s =
  printf "%s\n" (color_by_number (color_to_int color) s)

let res1 = color_print (Basic (Red,Bold)) "A bold red!"

let res2 = color_print (Gray 4) "A muted gray..."

let res3 = color_print (RGB (3, 2, 5)) "Some RGB color"

(*************)
(** Logging **)
(*************)

module Logging1 = struct
  module Log_entry = struct
    type t = { important: bool;
               message: string;
             }
  end

  module Heartbeat = struct
    type t = { status_message: string; }
  end

  module Logon = struct
    type t = { user: string;
               credentials: string;
             }
  end

  type details =
    | Logon of Logon.t
    | Heartbeat of Heartbeat.t
    | Log_entry of Log_entry.t

  module Common = struct
    type t = { session_id: string;
               time: Time_ns.t;
             }
  end

  let messages_for_user user (messages : (Common.t * details) list) =
    let (user_messages,_) =
      List.fold messages ~init:([],String.Set.empty)
        ~f:(fun ((messages,user_sessions) as acc) ((common,details) as message) ->
            match details with
            | Logon m ->
              if m.Logon.user = user then
                (message::messages,
                 Set.add user_sessions common.Common.session_id)
              else acc
            | Heartbeat _ | Log_entry _ ->
              if Set.mem user_sessions common.Common.session_id then
                (message::messages, user_sessions)
              else acc
          )
    in
    List.rev user_messages

end

module Logging2 = struct
  include Logging1

  type details' =
    | Logon     of { user: string; credentials: string; }
    | Heartbeat of { status_message: string; }
    | Log_entry of { important: bool; message: string; }

  let messages_for_user user (messages : (Common.t * details') list) =
    let (user_messages,_) =
      List.fold messages ~init:([],String.Set.empty)
        ~f:(fun ((messages,user_sessions) as acc) ((common,details) as message) ->
            match details with
            | Logon m ->
              if m.user = user then
                (message::messages, Set.add user_sessions common.Common.session_id)
              else acc
            | Heartbeat _ | Log_entry _ ->
              if Set.mem user_sessions common.Common.session_id then
                (message::messages, user_sessions)
              else acc
          )
    in
    List.rev user_messages
end

module Logging3 = struct
  module Log_entry = Logging1.Log_entry
  module Heartbeat = Logging1.Heartbeat
  module Logon = Logging1.Logon
  module Common = Logging1.Common

  type details =
    | Logon of { user: string; credentials: string; }
    | Heartbeat of { status_message: string; }
    | Log_entry of { important: bool; message: string; }

  let messages_for_user user (messages : (Common.t * details) list) =
    let (user_messages,_) =
      List.fold messages ~init:([],String.Set.empty)
        ~f:(fun ((messages,user_sessions) as acc) ((common,details) as message) ->
            match details with
            | Logon m ->
              if m.user = user then
                (message::messages, Set.add user_sessions common.Common.session_id)
              else acc
            | Heartbeat _ | Log_entry _ ->
              if Set.mem user_sessions common.Common.session_id then
                (message::messages, user_sessions)
              else acc
          )
    in
    List.rev user_messages
end

let res4 = Logging3.Log_entry.{message = "hello"; important = true}

type 'a expr =
  | Base  of 'a
  | Const of bool
  | And   of 'a expr list
  | Or    of 'a expr list
  | Not   of 'a expr

type mail_field = To | From | CC | Date | Subject

type mail_predicate =
  { field: mail_field;
    contains: string
  }

let test (field : mail_field) (contains : string) : mail_predicate expr =
  Base { field; contains }

let res5 =
  And
    [ Or
        [ test To "doligez";
          test CC "doligez"
        ];
      test Subject "runtime";
    ]

let rec eval (expr : 'a expr) (base_eval : 'a -> bool) : bool =
  (* a shortcut, so we don't need to repeatedly pass [base_eval]
     explicitly to [eval] *)
  let eval' expr = eval expr base_eval in
  match expr with
  | Base  base  -> base_eval base
  | Const bool  -> bool
  | And   exprs -> List.for_all exprs ~f:eval'
  | Or    exprs -> List.exists  exprs ~f:eval'
  | Not   expr  -> not (eval' expr)

let and_ l : 'a expr =
  if List.mem l (Const false) ~equal:(=)
  then Const false
  else
    match List.filter l ~f:((<>) (Const true)) with
    | [] -> Const true
    | [ x ] -> x
    | l -> And l

let or_ l =
  if List.mem l (Const true) ~equal:(=)
  then Const true
  else
    match List.filter l ~f:((<>) (Const false)) with
    | [] -> Const false
    | [x] -> x
    | l -> Or l

let not_ = function
  | Const b -> Const (not b)
  | Not e -> e
  | (Base _ | And _ | Or _ ) as e -> Not e

let rec simplify = function
  | Base _ | Const _ as x -> x
  | And l -> and_ (List.map ~f:simplify l)
  | Or l  -> or_  (List.map ~f:simplify l)
  | Not e -> not_ (simplify e)

let res6 =
  simplify
    (Not
       (And
          [ Or
              [ Base "it's snowing";
                Const true
              ];
            Base "it's raining"
          ]
       )
    )

let three : [> `Int of int] = `Int 3

let four : [> `Float of float] = `Float 4.

let nan : [> `Not_a_number] = `Not_a_number

let res7 : [> `Float of float | `Int of int | `Not_a_number] list =
  [three; four; nan]

let is_positive : [< `Float of float | `Int of int] -> bool = function
  | `Int   x -> x > 0
  | `Float x -> x > 0.

let is_positive2 : [> `Float of float | `Int of int] -> bool = function
  | `Int   x -> x > 0
  | `Float x -> x > 0.
  | _ -> false

let exact : [`Float of float | `Int of int] list = List.filter ~f:is_positive [three;four]

let exact2 = List.filter ~f:is_positive [three;four]

let exact3 = List.filter ~f:is_positive2 [three;four]

let is_positive3 = function
    | `Int   x -> Ok (x > 0)
    | `Float x -> Ok (x > 0.)
    | `Not_a_number -> Error "not a number"

let res8 =
  List.filter
    [three; four]
    ~f:(fun x ->
        match is_positive3 x with
        | Error _ -> false
        | Ok b -> b
      )

module PolyVarColor = struct

  let basic_color_to_int :
    [< `Black
    | `Blue
    | `Cyan
    | `Green
    | `Magenta
    | `Red
    | `White
    | `Yellow
    ] -> int =
    function
    | `Black -> 0 | `Red     -> 1 | `Green -> 2 | `Yellow -> 3
    | `Blue  -> 4 | `Magenta -> 5 | `Cyan  -> 6 | `White  -> 7

  let color_to_int :
    [< `Basic of
         [< `Black
         | `Blue
         | `Cyan
         | `Green
         | `Magenta
         | `Red
         | `White
         | `Yellow
         ] *
         [< `Bold | `Regular ]
    | `RGB of int * int * int
    | `Gray of int
    ] -> int
    = function
    | `Basic (basic_color,weight) ->
        let base = match weight with `Bold -> 8 | `Regular -> 0 in
        base + basic_color_to_int basic_color
    | `RGB (r,g,b) -> 16 + b + g * 6 + r * 36
    | `Gray i -> 232 + i

  let extended_color_to_int :
    [< `RGBA of int * int * int * int
    | `Basic of
         [< `Black
         | `Blue
         | `Cyan
         | `Green
         | `Magenta
         | `Red
         | `White
         | `Yellow
         ] *
         [< `Bold | `Regular ]
    | `RGB of int * int * int
    | `Gray of int
    ] -> int =
    function
    | `RGBA (r,g,b,a) -> 256 + a + b * 6 + g * 36 + r * 216
    (* Note that we can't use a catch-all case here, because then color_to_int
     * wouldn't be passed something of the correct type. *)
    | (`Basic _ | `RGB _ | `Gray _) as color -> color_to_int color

  (* This doesn't work because the pattern match can't limit the type of color. *)
  (* let extended_color_to_int' : *)
  (*   [< `RGBA of int * int * int * int *)
  (*   | `Basic of *)
  (*        [< `Black *)
  (*        | `Blue *)
  (*        | `Cyan *)
  (*        | `Green *)
  (*        | `Magenta *)
  (*        | `Red *)
  (*        | `White *)
  (*        | `Yellow *)
  (*        ] * *)
  (*        [< `Bold | `Regular ] *)
  (*   | `RGB of int * int * int *)
  (*   | `Gray of int *)
  (*   ] -> int = function *)
  (*   | `RGBA (r,g,b,a) -> 256 + a + b * 6 + g * 36 + r * 216 *)
  (*   (\* We can use a catch-all case here because the type signature limits what *)
  (*    * the type of color can be. *\) *)
  (*   | color -> color_to_int color *)
end
