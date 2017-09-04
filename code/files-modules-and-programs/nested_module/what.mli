
open Core

module type ID = sig
  type t
  val of_string : string -> t
  val to_string : t -> string
end

module Username : ID
