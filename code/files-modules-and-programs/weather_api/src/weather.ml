
open Core

type forecasts = Forecast.t list
[@@deriving show]

let parse_forecast forecast_json =
  let open Yojson.Basic.Util in
  let low_int = forecast_json |> member "low" |> to_string |> Int.of_string in
  let high_int = forecast_json |> member "high" |> to_string |> Int.of_string in
  let date_str = forecast_json |> member "date" |> to_string in
  let weather = forecast_json |> member "text" |> to_string in
  let date = Date.parse ~fmt:"%d %b %Y" date_str in
  Forecast.create
    date
    (Temp.create low_int Temp.Scale.Celcius)
    (Temp.create high_int Temp.Scale.Celcius)
    weather

let parse_forecasts item_json =
  let open Yojson.Basic.Util in
  let forecasts_json = item_json |> member "forecast" in
  let forecasts_json_list = forecasts_json |> to_list in
  let forecasts = List.map ~f:parse_forecast forecasts_json_list in
  forecasts

(* TODO: Use a function with an optional argument that defaults to today to get
   the forecast for an arbitrary day. *)

let time_zone_str_japan = Time.Zone.to_string @@ force Time.Zone.local

let todays_date : Date.t = Date.today (force Time.Zone.local)

(* You can find the format supported in strptime *)
let try_parse : Date.t = Date.parse ~fmt:"%d %b %Y" "02 Sep 2017"

let find_forecast ?date forecasts =
  let date =
    match date with
    | None -> Date.today (force Time.Zone.local)
    | Some date -> date
  in List.find forecasts ~f:(fun forecast -> Forecast.date forecast = date)

let get_forecasts () =
  let body = Lwt_main.run Request.call in
  let json = Yojson.Basic.from_string body in
  let item_json = Item.parse json in
  let forecasts = parse_forecasts item_json in
  forecasts

let try_find_forecast forecasts =
  let date = Date.parse ~fmt:"%Y-%m-%d" "2017-09-10"
  in find_forecast forecasts ~date

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
  print_endline @@ show_forecasts forecasts
