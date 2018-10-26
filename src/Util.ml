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


let decodeTranslations json : (string Js.Dict.t) Types.remoteData =
  let decode json =
    let open Json.Decode in
    let translations = json |> (dict string) in
    Types.Success translations
  in

  json
  |> Json.parseOrRaise
  |> decode

let loadTranslations lan callback =
  let url = Printf.sprintf "/translations/%s.json" lan in
  let open Tea in
  Http.send callback (Http.getString url)

let updateTranslations current append =
  match current with
  | Types.Success translations ->
     Array.iter (fun (k,v) -> Js.Dict.set translations k v) append
  | _ -> ()
     
let trans key translations =
  match translations with
  | Types.Success t ->
     begin
       match (Js.Dict.get t key) with
       | Some txt -> txt
       | _ -> key
     end            
  | _ -> key

  
let storageKey = "little-search"
                 
let toStorage k v =
  let open Dom.Storage in
  let ls = localStorage in
  setItem k v ls

let fromStorage k default =
  let open Dom.Storage in
  let ls = localStorage in
  match (getItem k ls) with
  | Some data -> data
  | _ -> default
  
let resetPageScroll _ =
  [%bs.raw
      {| document.documentElement.scrollTop = 0 |}
  ];
