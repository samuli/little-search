let extractSearchParams params =
  let lookfor = match List.find (fun (key, _value) -> key = "lookfor") params with
  | (_, lookfor) -> Js_global.decodeURIComponent lookfor
  | exception Not_found -> ""
  in
  let page = match List.find (fun (key, _value) -> key = "page") params with
  | (_, page) -> page
  | exception Not_found -> "0"
  in
  let limit = match List.find (fun (key, _value) -> key = "limit") params with
  | (_, limit) -> limit
  | exception Not_found -> "10"
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
  (lookfor, filters, page, limit)

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
  let url = Printf.sprintf "translations/%s.json" lan in
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

let scrollToElement id =
  let rec act id =
    let el = Web.Document.getElementById id in
    match Js.Nullable.toOption el with
    | None ->
       let _ = Web.Window.requestAnimationFrame (fun _ -> act id) in
       ()
    | Some _ ->
       [%bs.raw "window.scrollTo(0, Math.max(0, document.getElementById(id).offsetTop)-120)" ]
  in
  act id
    
let resetPageScroll _ =
  [%bs.raw
      {| document.documentElement.scrollTop = 0 |}
  ]

let hash: (string -> string ) = [%bs.raw fun s -> "
  return s.split(\"\").reduce(
     function(a,b){
       a=((a<<5)-a)+b.charCodeAt(0);
       return a&a
     },
   0);
"]

                              
external toLocaleString : int -> string = "" [@@bs.send]


                                      


    

