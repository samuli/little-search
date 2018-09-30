open Types
   
open Tea
   
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
   (* facets: option(Js.Dict.t(array(facetItem))), *)
  resultCount: int
}

type recordResult = {
  record: record;
  resultCount: int
}


let apiUrl = "https://api.finna.fi/api/v1"
let limit = 30

let getFieldQuery fields =
  List.map (fun f -> "&field[]=" ^ f) ["id"; "title"] |> String.concat ""
  
let getSearchUrl ~lookfor =
  let fields = getFieldQuery ["id"; "title"] in
  Printf.sprintf "%s/search?lookfor=%s%s&limit=%d" apiUrl lookfor fields limit

let getRecordUrl ~id =
  let fields = getFieldQuery ["id"; "title"; "authors"] in
  Printf.sprintf "%s/record?id=%s%s" apiUrl id fields

(* Decoders *)
let recordDecoder =
  let record id title = { id; title } in
  let open Tea.Json.Decoder in
  map2 record
    (field "id" string)
    (field "title" string)

let decodeSearchResults json =
  let results resultCount records =
    { resultCount; records }
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
