
let parse json =
  let open Yojson.Basic.Util in
  let query = json |> member "query" in
  let results = query |> member "results" in
  let channel = results |> member "channel" in
  let item = channel |> member "item" in
  (* let condition = item |> member "condition" in *)
  item
