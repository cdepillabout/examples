open Core

module Api_key = struct
  type t = string

  let to_string t = t

  let env_var_name = "ALPHA_VANTAGE_API_KEY"

  let read_from_env =
    match Sys.getenv "ALPHA_VANTAGE_API_KEY" with
    | Some env_var -> env_var
    | None ->
      printf
        "WARNING: You must specify the Alpha Vantage API Key in the environment \
         variable \n\"%s\".  Since it is not specified, using the API Key \
         \"demo\".\n"
        env_var_name ;
      "demo"
end

module Symbol = struct
  type t = BTC | ETH

  let to_string = function
    | BTC -> "BTC"
    | ETH -> "ETH"
end

(* https://www.alphavantage.co/query?function=DIGITAL_CURRENCY_DAILY&symbol=BTC&market=CNY&apikey=demo *)

module Api = struct

  let make_uri params =
    Uri.make
      ~scheme:"https"
      ~host:"www.alphavantage.co"
      ~path:"query"
      ~query:params
      ()

  module Digital_currency = struct
    module Daily = struct
      let uri api_key symbol =
        make_uri
          [ ("apikey", [api_key]);
            ("function", ["DIGITAL_CURRENCY_DAILY"]);
            ("market", ["USD"]);
            ("symbol", [Symbol.to_string symbol]);
          ]

      let get api_key symbol =
        let open Cohttp_async in
        Client.get (uri api_key symbol)
    end
  end
end

let api_key = Api_key.read_from_env

(* let () = printf "%s\n" api_key *)

let () =
  Async.run ();
  printf "hello\n";
  Async.printf "hello2\n";
  (* let open Cohttp_async in *)
  (* Api.Digital_currency.Daily.get api_key Symbol.ETH Async.>>> fun (resp, body) -> *)
  (* Body.to_string body Async.>>> (fun body_str -> *)
  (*     printf "resp: %s\n" (Sexp.to_string (Response.sexp_of_t resp)); *)
  (*     printf "body: %s\n" body_str) *)

