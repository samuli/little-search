open Types

type filter = {
    key: string;
    value: string
  }

type facetType = | FacetNormal | FacetBoolean
                 
type facetItem = {
    value: string;
    label: string;
    count: int
  }
           
type record = {
  id: string;
  title: string;
  (* formats: option(array(translated)),
   * buildings: option(array(translated)),
   * images: array(string),
   * authors: array(string),
   * publishers: option(array(string)),
   * year: option(string),
   * onlineUrls: option(array(onlineUrl)),
   * urls: option(array(onlineUrl)), *)
  }

type searchResult = {
  records: record array;
  resultCount: int
}

type facetResult = {
  facets: facetItem array Tea.Json.Decoder.ObjectDict.t;
}
                  
type recordResult = {
  record: record;
  resultCount: int
}

let apiUrl = "https://api.finna.fi/api/v1"

let getFieldQuery _fields =
  List.map (fun f -> "&field[]=" ^ f) ["id"; "title"] |> String.concat ""

let getSearchUrl ~lookfor ~page ~limit ~filters =
  let fields = getFieldQuery ["id"; "title"] in
  let filters = (Array.map (fun filter -> Printf.sprintf "filter[]=%s:%s" filter.key filter.value ) filters) |> Array.to_list |> String.concat "&" in
  Printf.sprintf "%s/search?lookfor=%s%s&limit=%d&page=%d&%s" apiUrl lookfor fields limit page filters

let getFacetSearchUrl ~lookfor ~page ~facet =
  Printf.sprintf "%s/search?lookfor=%s&limit=0&page=%d&facet[]=%s" apiUrl lookfor page facet
  
let getRecordUrl ~id =
  let fields = getFieldQuery ["id"; "title"; "authors"] in
  Printf.sprintf "%s/record?id=%s%s" apiUrl id fields

(* Decoders *)
let facetDecoder =
  let facet value label count = { value; label; count } in
  let open Tea.Json.Decoder in
  map3 facet
    (field "value" string)
    (field "translated" string)
    (field "count" int)
  
let recordDecoder =
  let record id title = { id; title } in
  let open Tea.Json.Decoder in
  map2 record
    (field "id" string)
    (field "title" string)

let decodeSearchResults json =
  let results resultCount records =
    { resultCount; records}
  in
  let open Tea.Json.Decoder in
  let resultDecoder = map2 results
                        (field "resultCount" int)
                        (field "records" (array recordDecoder))

  in
  begin match (decodeString resultDecoder json) with
  | Ok results -> Success results
  | Error e -> Error e
  end

let decodeFacetResults json =
  let results facets =
    {facets}
  in
  let open Tea.Json.Decoder in
  let resultDecoder = map results
                        (field "facets" (dict (array facetDecoder)))

  in
  begin match (decodeString resultDecoder json) with
  | Ok results -> begin
      let keyVals = ObjectDict.bindings results.facets in
      Success (List.hd keyVals)
    end
  | Error e -> Error e
  end

let decodeRecordResult json =
  let results resultCount records =
    { resultCount; record = records.(0) }
  in
  let open Tea.Json.Decoder in
  let resultDecoder = map2 results
                        (field "resultCount" int)
                        (field "records" (array recordDecoder))
  in
  begin match (decodeString resultDecoder json) with
  | Ok results -> Success results
  | Error e -> Error e
  end
