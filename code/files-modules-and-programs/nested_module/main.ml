
open Core

module type ID = sig
  type t
  val of_string : string -> t
  val to_string : t -> string
end

module String_id : ID = struct
  type t = string
  let of_string x = x
  let to_string x = x
end

module Firstname : ID = String_id
module Lastname : ID = String_id

let username_len (username : What.Username.t) =
  let open What.Username in
  String.length (to_string username)

let firstname_len (firstname : Firstname.t) =
  Firstname.(String.length (to_string firstname))

let lastname_length (lastname : Lastname.t) =
  let module L = Lastname in
  String.length (L.to_string lastname)

let () = ()
