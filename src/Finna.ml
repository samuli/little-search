open Types

            
type onlineUrl = {
  url: string option;
  label: string option;
}

type facetType = | FacetNormal | FacetBoolean
                 
type facetItem = {
    value: string;
    translated: string;
    count: int
  }

type translated = {
  value: string;
  translated: string
  }
                
type record = {
  id: string;
  title: string;
  formats: translated array option;
  images: string array option;
  authors: string array option;
  buildings: translated array option;
  publishers: string array option;
  year: string option;
  onlineUrls: onlineUrl array option;
  urls: onlineUrl array option;
  summary: string array option;
  }

type searchResultRaw = {
  records: record array option;
  resultCount: int option;
  status: string;    
  }
                     
type searchResult = {
  records: record array;
  resultCount: int;
  }


type facetResultRaw = {
  facets: facetItem array Js.Dict.t option;
  status: string;    
  }
type facetResult = {
  facets: facetItem array Js.Dict.t option;
}

let baseUrl = "https://api.finna.fi"
let apiUrl = baseUrl ^ "/api/v1"

let getRecordLink id =
  Printf.sprintf "https://finna.fi/Record/%s" id
     
let getFieldQuery _fields =
  let fields = ["id"; "title"; "formats"; "images"; "authors"; "buildings"; "publishers"; "year"; "urls"; "onlineUrls"; "summary"] in
  List.map (fun f -> "&field[]=" ^ f) fields |> String.concat ""

let getFilterQuery filters =
  (Array.map (fun filter -> Printf.sprintf "filter[]=%s:%s" filter.key filter.value ) filters) |> Array.to_list |> String.concat "&"
  
let getSearchUrl params =
  let fields = getFieldQuery ["id"; "title"] in
  let filters = getFilterQuery params.filters in
  Printf.sprintf "%s/search?lookfor=%s%s&limit=%d&page=%d&%s" apiUrl params.lookfor fields params.limit params.page filters

let getFacetSearchUrl ~facet ~params =
  let filters = getFilterQuery params.filters in
  Printf.sprintf "%s/search?lookfor=%s&limit=0&page=%d&facet[]=%s&%s" apiUrl params.lookfor params.page facet filters
  
let getRecordUrl ~id =
  let id = Js_global.encodeURIComponent id in
  let fields = getFieldQuery ["id"; "title"; "authors"] in
  Printf.sprintf "%s/record?id=%s%s" apiUrl id fields

(* Decoders *)
let urlDecoder json =
  let open Json.Decode in
  {
    url = json |> optional (field "url" string);
    label = json |> optional (field "label" string);
  }

let facetDecoder json : facetItem =
  let open Json.Decode in
  let labelDecoder =
    either
      (string |> map (fun s -> s))
      (int |> map (fun i -> string_of_int i))
  in
  {
    value = json |> field "value" labelDecoder;
    translated = json |> field "translated" labelDecoder;
    count = json |> field "count" int;
  }

let translatedDecoder json : translated =
  let open Json.Decode in
  {
    value = json |> field "value" string;
    translated = json |> field "translated" string;
  }
                   
let recordDecoder json =
  let open Json.Decode in
  {
    id = json |> field "id" string;
    title = json |> field "title" string;
    formats = json |> (optional (field "formats" (array translatedDecoder)));
    buildings = json |> (optional (field "buildings" (array translatedDecoder)));
    images = json |> (optional (field "images" (array string)));
    publishers = json |> (optional (field "publishers" (array string)));
    year = json |> (optional (field "year" string));
    authors =
      json
      |> [%bs.raw
             {| (json) => { return(Object.keys(json.authors.primary)); } |}
         ];
    onlineUrls = json |> (optional (field "onlineUrls" (array urlDecoder)));
    urls = json |> (optional (field "urls" (array urlDecoder)));
    summary = json |> (optional (field "summary" (array string)));
  }
  
let decodeSearchResults json : searchResult remoteData =
  let decode json =
    let open Json.Decode in
    {
      records = json |> (optional (field "records"  (array recordDecoder)));
      resultCount = json |> (optional (field "resultCount" int));
      status = json |> field "status" string;
    }
  in
  let process (results:searchResultRaw) =
    match results.status with
  | "OK" ->
     let (resultCount, records) = match (results.resultCount, results.records) with
       | (Some count, Some records) -> (count, records)
       | (_,_) -> (0, [||])
     in
     Success {
         records;
         resultCount
       }
  | _ -> Error "json"
  in

  json
  |> Json.parseOrRaise
  |> decode
  |> process

let decodeRecordResult json : record remoteData =
  let decode json =
    let open Json.Decode in
    {
      records = json |> (optional (field "records"  (array recordDecoder)));
      resultCount = json |> (optional (field "resultCount" int));
      status = json |> field "status" string;
    }
  in
  let process (results:searchResultRaw) =
    match results.status with
    | "OK" ->
       begin
         match (results.resultCount, results.records) with
         | (Some _count, Some records) -> Success records.(0)
         | (_,_) -> Error "Record not found"
       end
    | _ -> Error "Error parsing result"
  in

  json
  |> Json.parseOrRaise
  |> decode
  |> process

let decodeFacetResults json : (string * facetItem array) remoteData =
  let decode json =
    let open Json.Decode in
    {
      facets = json |> (optional (field "facets" (dict (array facetDecoder))));
      status = json |> field "status" string;
    }
  in
  let extractFacet facets =
    let keys = Js.Dict.keys facets |> Array.to_list in
    let key = List.hd keys in
    match (Js.Dict.get facets key) with
    | Some items -> Success (key, items)
    | _ -> Error "Facet error2"
  in
  let process results =
    match results.status with
    | "OK" ->
       begin
         match results.facets with
         | Some facets -> extractFacet facets
         | _ -> Error "Facet error"
       end
    | _ -> Error "Facet error"
  in

  json
  |> Json.parseOrRaise
  |> decode
  |> process
