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
