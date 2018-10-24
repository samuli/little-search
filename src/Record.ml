open Types
open View
open Tea
open Tea.Html


type msg =
  | ShowRecord of string
  | GotResult of (string, string Http.error) Result.t
  | CloseRecord
  | PageLoaded
[@@bs.deriving {accessors}]

type model = {
    record: Finna.record remoteData;
    nextRecord: Finna.record remoteData;
  }

let init =
  {
    record = NotAsked;
    nextRecord = NotAsked;
  }

let recordNeighbors id ids =
  let rec find id ids cnt =
    match ids with
    | [] -> None
    | hd :: rest ->
       if hd = id then Some cnt else find id rest (cnt+1)
  in
  let (prev, next, ind) = match find id ids 0 with
  | None -> (None, None, 0)
  | Some ind -> begin
     let ids = Array.of_list ids in
     let len = Js.Array.length ids in
     let prev = match ind with
       | 0 -> None
       | _ -> Some ids.(ind-1)
     in
     let next = match (len, ind, len-ind) with
       | (a, b, _) when a = b -> None
       | (_a, b, c) when c > 1 ->
          let ind = b+1 in Some ids.(ind)
       | _ -> None
     in
     (prev, next, ind)
    end
  in
  (prev,next, ind, List.length ids)
  
let update model context = function
  | ShowRecord id ->
     let url =
       Finna.getRecordUrl ~id ~lng:(Types.finnaLanguageCode context.language)
     in
     let cmd =  Http.send gotResult (Http.getString url) in
     ( { model with nextRecord = Loading }, cmd )
  | GotResult (Ok data) ->
     let record = Finna.decodeRecordResult data in
     ( { model with record }, Cmd.msg pageLoaded )
  | GotResult (Error e) ->
     ( { model with record = Error (Http.string_of_error e) }, Cmd.msg pageLoaded )
  |_ -> (model, Cmd.none)

let images recId imgs =
  (match imgs with
   | Some images when (Array.length images) = 0 -> noNode
   | Some images ->
      let item i path imgId =
        let path = Finna.baseUrl ^ path in
        let attrs =
          [ id imgId; class' Style.recordImage ] @
            (* load first images immediately and the rest with inView.js *)
            (if i < 4 then [ src path; noProp ] else [ noProp; href path ])
        in
        li [] [ img attrs [] ]
      in
      (* hash record.id to get a working querySelector for inView.js *)
      let hash =
        [%raw {| 
               function(s) {
               return s.split("").reduce(function(a,b){a=((a<<5)-a)+b.charCodeAt(0);
               return a&a},0);
               }|}]
      in
      let imgId i =
        let id = hash recId in
        "img-" ^ id ^ "-" ^ (string_of_int i)
      in
      
      let images =
        Array.mapi (fun i path ->
            let id = imgId i in
            item i path id) images
      in
      let ids = Array.mapi (fun i _ -> imgId i) images in
      Js.Global.setTimeout (fun () ->
          View.registerInview (Array.to_list ids)) 100 |> ignore;
      
      ul [ class' Style.recordImages ] (Array.to_list images)
   | None -> noNode)

let getFormats formats =
  match formats with
  | Some formats when Array.length formats > 0 ->
     let format:Finna.translated = formats.((Array.length formats)-1) in
     Some format.translated
  | _ -> None
       
let formats formats =
  match getFormats formats with
  | Some formats ->
     span [ class' Style.recordFormat ] [ text formats ]
  | None -> noNode

let getBuildings buildings =
  match buildings with
  | Some buildings when Array.length buildings > 0 ->
     let building:Finna.translated = buildings.(0) in
     Some building.translated
  | _ -> None
       
let buildings buildings =
  match getBuildings buildings with
  | Some buildings ->
     span [ class' Style.facetLink ] [ text buildings ]
  | None -> noNode

let getAuthors authors =
  match authors with
  | Some authors when Array.length authors > 0 ->
     Some (String.concat ", " (Array.to_list authors))
  | _ -> None

let authors authors =
  match getAuthors authors with
  | Some authors ->
     p [ class' Style.recordAuthors] [ text authors ]
  | None -> noNode

let getPublishInfo (r:Finna.record) =
  match (r.publishers, r.year) with
    | (Some publishers, Some year) when Array.length publishers > 0 ->
       Some (Printf.sprintf "%s %s" publishers.(0) year)
    | (Some publishers, None) when Array.length publishers > 0 ->
       Some publishers.(0)
    | (None, Some year) -> Some year
    | _ -> None

let publishInfo r =
  match getPublishInfo r with
  | Some info -> span [ class' Style.recordPublisher ] [ text info ]
  | _ -> noNode

let getSummary summary =
  match summary with
  | Some summary when Array.length summary > 0 ->
     Some (String.concat ". " (Array.to_list summary))
  | _ -> None

let summary summary =
  match getSummary summary with
  | Some summary ->
     p [ class' Style.recordSummary ] [ text summary ]
  | None -> noNode
          
let urlList (r:Finna.record) =
  let urls =
    (match r.urls with
     | Some urls when Array.length urls > 0 -> urls |> Array.to_list
     | _ -> []) @
      (match r.onlineUrls with
       | Some urls when Array.length urls > 0 -> urls |> Array.to_list
       | _ -> [])
  in
  ul [ class' Style.recordLinks ] (List.map (fun (url:Finna.onlineUrl) ->
      match (url.label, url.url) with
      | (Some label, Some url) -> li [ class' Style.recordLink ] [ a [ href url ] [ text label ] ]
      | (None, Some url) -> li [ class' Style.recordLink ] [ a [ href url ] [ text url ] ]
      | _ -> noNode) urls)

let finnaLink id context =
  p [] [ a [ href (Finna.getRecordLink id) ]
           [ text (Util.trans "View in Finna" context.translations) ] ]

let recordNavigation (record:Finna.record) context =
  let (prevId, nextId, ind, totCnt) = recordNeighbors record.id context.recordIds in
  div [ ] [
      (match prevId with
       | Some id ->
          a
            [ href (Router.routeToUrl (RecordRoute id)) ]
            [ text (Util.trans "Previous" context.translations) ]
       | None -> noNode )
    ; (if totCnt > 0 then
         p [] [ text (Printf.sprintf "%d / %d" (ind+1) totCnt) ]
       else
         noNode)
    ; (match nextId with
       | Some id ->
          a
            [ href (Router.routeToUrl (RecordRoute id)) ]
            [ text (Util.trans "Next" context.translations) ]
       | None -> noNode ) ]
  
let viewRecord (r:Finna.record) context =
  div [ ] [
      (recordNavigation r context)
    ; h1 [] [ text r.title ]
    ; summary r.summary
    ; authors r.authors
    ; publishInfo r
    ; div [] [
          formats r.formats
        ; buildings r.buildings
        ]
    ; images r.id r.images
    ; urlList r
    ; finnaLink r.id context
    ]
  
let view model context =
  div
    [ class' Style.recordFull ]
    [
      match model.record with
      | Loading -> statusLoading ()
      | Error e -> statusError e
      | Success r -> viewRecord r context
      | _ -> Html.noNode
    ]
