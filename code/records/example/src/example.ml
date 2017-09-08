
open Core

module Time_ns = Core.Time_ns

type service_info' =
  { service_name : string;
    port         : int;
    protocol     : string;
  }

let service_info_of_string line : service_info' =
  let matches =
    Re.exec (Re_posix.compile_pat "([a-zA-Z]+)[ \t]+([0-9]+)/([a-zA-Z]+)") line
  in
  { service_name = Re.get matches 1;
    port = Int.of_string (Re.get matches 2);
    protocol = Re.get matches 3;
  }

let ssh = service_info_of_string
    "ssh 22/udp # SSH Remote Login Protocol"

type 'a with_line_num = { item: 'a; line_num: int }

let parse_lines
    (parse : string -> 'a)
    (file_contents : string)
  : 'a with_line_num list =
  let lines = String.split ~on:'\n' file_contents in
  List.mapi
    lines
    ~f:(fun line_num line ->
        { item = parse line;
          line_num = line_num + 1;
        })

let res1 = parse_lines service_info_of_string
    "rtmp              1/ddp     # Routing Table Maintenance Protocol\n\
     tcpmux            1/udp     # TCP Port Service Multiplexer\n\
     tcpmux            1/tcp     # TCP Port Service Multiplexer"

let res2 = parse_lines Int.of_string "1\n10\n100\n1000"

let service_info_to_string { service_name = name; port = port; protocol = prot } =
  sprintf "%s %i/%s" name port prot

let res3 = service_info_to_string ssh

type service_info =
  { service_name : string;
    port         : int;
    protocol     : string;
    comment      : string option;
  }

let host_info_to_string { service_name = name; port = port; protocol = prot; _ } =
  sprintf "%s %i/%s" name port prot

let service_info_to_string { service_name; port; protocol; comment } =
  let base = sprintf "%s %i/%s" service_name port protocol in
  match comment with
  | None -> base
  | Some text -> base ^ " #" ^ text

let service_info_of_string line =
  (* first, split off any comment *)
  let (line,comment) =
    match String.rsplit2 line ~on:'#' with
    | None -> (line,None)
    | Some (ordinary,comment) -> (ordinary, Some comment)
  in
  (* now, use a regular expression to break up the service definition *)
  let matches =
    Re.exec (Re_posix.compile_pat "([a-zA-Z]+)[ \t]+([0-9]+)/([a-zA-Z]+)") line
  in
  let service_name = Re.get matches 1 in
  let port = Int.of_string (Re.get matches 2) in
  let protocol = Re.get matches 3 in
  { service_name; port; protocol; comment }

let ssh = service_info_of_string
    "ssh 22/udp # SSH Remote Login Protocol"

module Log_entry = struct
  type t =
    { session_id: string;
      time: Time_ns.t;
      important: bool;
      message: string;
    }
end

module Heartbeat = struct
  type t =
    { session_id: string;
      time: Time_ns.t;
      status_message: string;
    }
end

module Logon = struct
  type t =
    { session_id: string;
      time: Time_ns.t;
      user: string;
      credentials: string;
    }
  [@@deriving fields]
end

(* This is a weird way of accessing fields of a record. *)
let is_important t = t.Log_entry.important

(* Qualifying the record field by a module is not required if it can be inferred
   by the type. *)
let is_important' (t : Log_entry.t) = t.important

type client_info =
  { addr: Unix.Inet_addr.t;
    port: int;
    user: string;
    credentials: string;
    last_heartbeat_time: Time_ns.t;
  }

(* Functional updates of records with the "with" keyword. *)
let register_heartbeat t hb =
  { t with last_heartbeat_time = hb.Heartbeat.time }

(* Mutable record fields. *)
type client_info' =
  { addr: Unix.Inet_addr.t;
    port: int;
    user: string;
    credentials: string;
    mutable last_heartbeat_time: Time_ns.t;
    mutable last_heartbeat_status: string;
  }

(* Mutable record field update. *)
let register_heartbeat' t hb =
  t.last_heartbeat_time   <- hb.Heartbeat.time;
  t.last_heartbeat_status <- hb.Heartbeat.status_message
