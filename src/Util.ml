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
                    | [| key; value |] ->
                       (key, (Js_global.decodeURIComponent value))
                    | _ -> (value, "")) filters
  in
  (lookfor, filters)


(* let storageKey = "little-search"
 *                  
 * let saveSession data =
 *   let open Dom.Storage in
 *   let ls = localStorage in
 *   setItem storageKey data ls
 * 
 * let restoreSession =
 *   let open Dom.Storage in
 *   let ls = localStorage in
 *   match getItem storageKey ls with
 *     | Some item -> item
 *     | _ -> "None" *)
  
