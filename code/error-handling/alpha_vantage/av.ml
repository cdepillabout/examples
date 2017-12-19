open Core

let api_key_env_var_name = "ALPHA_VANTAGE_API_KEY"

let get_api_key =
  match Sys.getenv "ALPHA_VANTAGE_API_KEY" with
  | Some env_var -> env_var
  | None ->
    printf
      "WARNING: You must specify the Alpha Vantage API Key in the environment \
       variable \n\"%s\".  Since it is not specified, using the API Key \
       \"demo\".\n"
      api_key_env_var_name ;
    "demo"



let () = printf "%s\n" get_api_key
