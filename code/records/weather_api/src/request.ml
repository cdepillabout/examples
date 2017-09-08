
open Core

let weather_url = "https://query.yahooapis.com/v1/public/yql?q=select%20*%20from%20weather.forecast%20where%20woeid%20in%20(select%20woeid%20from%20geo.places(1)%20where%20text%3D%22tokyo%2C%20japan%22)%20and%20u%20%3D%20%22c%22&format=json&env=store%3A%2F%2Fdatatables.org%2Falltableswithkeys&callback="

let call =
  let open Lwt in
  Cohttp_lwt_unix.Client.get (Uri.of_string weather_url) >>= fun (resp, body) ->
  body |> Cohttp_lwt.Body.to_string
