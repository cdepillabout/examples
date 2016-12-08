open Core.Std

module type ID = sig
  type t
  val of_string : string -> t
  val to_string : t -> string
end

module String_id = struct
  type t = string
  let of_string x = x
  let to_string x = x
end

module Username : ID = String_id
module Hostname : ID = String_id

type session_info = { user: Username.t;
                      host: Hostname.t;
                      when_started: Time.t;
                    }

let sessions_have_same_user s1 s2 =
  (* This is incorrect: *)
  (* s1.user = s2.host *)
  Username.to_string s1.user = Hostname.to_string s2.host

module What = struct
  type t = string
  let who = 3
  let where = 4
  let (ga : t) = "hehehe"
end

let lala =
  What.who + What.where

(* example of a local open *)
let test =
  let open What in
  who + where

let average x y =
  Int64.(x + y / of_int 2)
