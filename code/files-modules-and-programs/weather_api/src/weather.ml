
open Core

let weather_url = "https://query.yahooapis.com/v1/public/yql?q=select%20*%20from%20weather.forecast%20where%20woeid%20in%20(select%20woeid%20from%20geo.places(1)%20where%20text%3D%22tokyo%2C%20japan%22)%20and%20u%20%3D%20%22c%22&format=json&env=store%3A%2F%2Fdatatables.org%2Falltableswithkeys&callback="

let do_request =
  let open Lwt in
  Cohttp_lwt_unix.Client.get (Uri.of_string weather_url) >>= fun (resp, body) ->
  body |> Cohttp_lwt.Body.to_string

type current_weather =
  { temp: Temp.t;
    weather: string
  }
[@@deriving show]

let current_weather_to_string {temp; weather} =
  Temp.to_string temp ^ " " ^ weather

type forecast =
  { date: Date.t;
    high: Temp.t;
    low: Temp.t;
    weather: string;
  }
[@@deriving show]

type forecasts = forecast list
[@@deriving show]

let parse_item_json json =
  let open Yojson.Basic.Util in
  let query = json |> member "query" in
  let results = query |> member "results" in
  let channel = results |> member "channel" in
  let item = channel |> member "item" in
  (* let condition = item |> member "condition" in *)
  item

let parse_current_weather item_json =
  let open Yojson.Basic.Util in
  let condition = item_json |> member "condition" in
  let temp_int = condition |> member "temp" |> to_string |> Int.of_string in
  let weather = condition |> member "text" |> to_string in
  let temp = Temp.create temp_int Temp.Scale.Celcius in
  { temp; weather }

let parse_forecast forecast_json =
  let open Yojson.Basic.Util in
  let low_int = forecast_json |> member "low" |> to_string |> Int.of_string in
  let high_int = forecast_json |> member "high" |> to_string |> Int.of_string in
  let date_str = forecast_json |> member "date" |> to_string in
  let weather = forecast_json |> member "text" |> to_string in
  let date = Date.parse ~fmt:"%d %b %Y" date_str in
  { date;
    weather;
    low = Temp.create low_int Temp.Scale.Celcius;
    high = Temp.create high_int Temp.Scale.Celcius;
  }

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
  in List.find forecasts ~f:(fun forecast -> forecast.date = date)

let get_forecasts () =
  let body = Lwt_main.run do_request in
  let json = Yojson.Basic.from_string body in
  let item_json = parse_item_json json in
  let forecasts = parse_forecasts item_json in
  forecasts

let try_find_forecast forecasts =
  let date = Date.parse ~fmt:"%Y-%m-%d" "2017-09-10"
  in find_forecast forecasts ~date

let run () =
  let body = Lwt_main.run do_request in
  let json = Yojson.Basic.from_string body in
  print_endline @@ "json: " ^ (Yojson.Basic.pretty_to_string json) ;
  let item_json = parse_item_json json in
  (* print_endline @@ "weather: " ^ (Yojson.Basic.pretty_to_string current_weather_json); *)
  let current_weather = parse_current_weather item_json in
  let forecasts = parse_forecasts item_json in
  (* printf "%s\n" (current_weather_to_string current_weather) *)
  print_endline @@ show_current_weather current_weather ;
  print_endline @@ show_forecasts forecasts
