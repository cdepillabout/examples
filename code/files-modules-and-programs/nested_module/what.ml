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

module Username : ID = String_id
module Firstname : ID = String_id
module Lastname : ID = String_id
