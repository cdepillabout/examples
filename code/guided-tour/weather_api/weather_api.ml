open Lwt
open Cohttp
open Cohttp_lwt_unix

let url = "https://google.com"

let do_request =
  Client.get (Uri.of_string url) >>= fun (resp, body) ->
  body |> Cohttp_lwt.Body.to_string

let run () =
  let body = Lwt_main.run do_request in
  print_endline ("Received body\n" ^ body)
