
open Core

type forecasts = Forecast.t list
[@@deriving show]

let parse_forecasts item_json =
  let open Yojson.Basic.Util in
  let forecasts_json = item_json |> member "forecast" in
  let forecasts_json_list = forecasts_json |> to_list in
  let forecasts = List.map ~f:Forecast.parse forecasts_json_list in
  forecasts

let get_forecasts () =
  let body = Lwt_main.run Request.call in
  let json = Yojson.Basic.from_string body in
  let item_json = Item.parse json in
  let forecasts = parse_forecasts item_json in
  forecasts

let try_find_forecast forecasts = Forecast.find_raw_date "2017-09-10" forecasts

let run () =
  let body = Lwt_main.run Request.call in
  let json = Yojson.Basic.from_string body in
  print_endline @@ "json: " ^ (Yojson.Basic.pretty_to_string json) ;
  let item_json = Item.parse json in
  (* print_endline @@ "weather: " ^ (Yojson.Basic.pretty_to_string current_weather_json); *)
  let current_weather = Current.parse item_json in
  let forecasts = parse_forecasts item_json in
  (* printf "%s\n" (current_weather_to_string current_weather) *)
  print_endline @@ Current.show current_weather ;
  print_endline @@ show_forecasts forecasts ;
  match Forecast.find_raw_date "2017-09-10" forecasts with
  | None -> print_endline "Could not find a forecast for date 2017-09-10."
  | Some forecast ->
    print_endline "Found forecast for date 2017-09-10:" ;
    print_endline @@ Forecast.show forecast
