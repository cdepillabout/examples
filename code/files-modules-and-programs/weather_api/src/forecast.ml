open Core

type t =
  { date: Date.t;
    high: Temp.t;
    low: Temp.t;
    weather: string;
  }
[@@deriving show]

let create date high low weather = {date; high; low; weather}

let date forecast = forecast.date

let parse forecast_json =
  let open Yojson.Basic.Util in
  let low_int = forecast_json |> member "low" |> to_string |> Int.of_string in
  let high_int = forecast_json |> member "high" |> to_string |> Int.of_string in
  let date_str = forecast_json |> member "date" |> to_string in
  let weather = forecast_json |> member "text" |> to_string in
  let date = Date.parse ~fmt:"%d %b %Y" date_str in
  create
    date
    (Temp.create low_int Temp.Scale.Celcius)
    (Temp.create high_int Temp.Scale.Celcius)
    weather

