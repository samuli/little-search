let extractSearchParams params =
  let lookfor = match List.find (fun (key, _value) -> key = "lookfor") params with
  | (_, lookfor) -> lookfor
  | exception Not_found -> ""
  in
  let filters =
    List.filter (fun (key, _value) -> key = "filter[]") params
  in
  let filters = List.map (fun (_key, value) ->
                    match Js.String.split ":" value with
                    | [| key; value |] -> (key, value)
                    | _ -> (value, "")) filters
  in
  (lookfor, filters)
       
