
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
      sprintf "\027[38;5;%dm%s\027[0m" number text;;


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
