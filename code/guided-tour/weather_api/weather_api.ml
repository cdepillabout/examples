open Cohttp
open Cohttp_lwt_unix
open Core
open Lwt
open Yojson

let weather_url = "https://query.yahooapis.com/v1/public/yql?q=select%20*%20from%20weather.forecast%20where%20woeid%20in%20(select%20woeid%20from%20geo.places(1)%20where%20text%3D%22tokyo%2C%20japan%22)%20and%20u%20%3D%20%22c%22&format=json&env=store%3A%2F%2Fdatatables.org%2Falltableswithkeys&callback="

let do_request =
  Client.get (Uri.of_string weather_url) >>= fun (resp, body) ->
  (* let code = resp |> Response.status |> Code.code_of_status in *)
  (* Printf.printf "Response code: %d\n" code; *)
  (* Printf.printf "Headers: %s\n" (resp |> Response.headers |> Header.to_string); *)
  body |> Cohttp_lwt.Body.to_string >|= fun body ->
  (* Printf.printf "Body of length: %d\n" (String.length body); *)
  body

type temp_scale = Fahrenheit | Celcius
  [@@deriving show]

type temp =
  { temp: int;
    scale: temp_scale
  }
  [@@deriving show]

type current_weather =
  { temp: temp;
    weather: string
  }
  [@@deriving show]

let temp_to_string {temp; scale} =
  match scale with
  | Fahrenheit -> string_of_int temp ^ "F"
  | Celcius -> string_of_int temp ^ "C"

let current_weather_to_string {temp; weather} =
  temp_to_string temp ^ " " ^ weather

let parse_json str = Yojson.Basic.from_string str

let get_current_weather_json json =
  let open Yojson.Basic.Util in
  let query = json |> member "query" in
  let results = query |> member "results" in
  let channel = results |> member "channel" in
  let item = channel |> member "item" in
  let condition = item |> member "condition" in
  condition

let parse_current_weather json =
  let open Yojson.Basic.Util in
  let temp_int = json |> member "temp" |> to_string |> Int.of_string in
  let weather = json |> member "text" |> to_string in
  let temp = { temp = temp_int; scale = Celcius } in
  { temp; weather }

let run () =
  let body = Lwt_main.run do_request in
  (* print_endline ("Received body\n" ^ body) *)
  let json = parse_json body in
  (* print_endline @@ "json: " ^ (Yojson.Basic.pretty_to_string json) ; *)
  let current_weather_json = get_current_weather_json json in
  (* print_endline @@ "weather: " ^ (Yojson.Basic.pretty_to_string current_weather_json); *)
  let current_weather = parse_current_weather current_weather_json in
  (* printf "%s\n" (current_weather_to_string current_weather) *)
  print_endline @@ show_current_weather current_weather
