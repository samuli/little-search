open Types
open View
open Tea
open Tea.Html


type msg =
  | ShowRecord of string
  | RecordPaginate of (resultpageNum * navigateCmd)
  | RecordPaginated
  | NextResultPageLoaded of (int, string) Result.t
  | LoadSearchResults of int
  | GotResult of (string, string Http.error) Result.t
  | CloseRecord
[@@bs.deriving {accessors}]

type model = {
    record: Types.record remoteData;
    nextRecord: Types.record remoteData;
    navigateCmd: navigateCmd;
  }

let init =
  {
    record = NotAsked;
    nextRecord = NotAsked;
    navigateCmd = NoNavigate;
  }

let update ~model ~context ~(results:searchResultsType) = function
  | ShowRecord id ->
     let url =
       Finna.getRecordUrl ~id ~lng:(Types.finnaLanguageCode context.language)
     in
     let cmd =  Http.send gotResult (Http.getString url) in
     ( { model with nextRecord = Loading }, cmd, NoUpdate )
  | GotResult (Ok data) ->
     let record = Finna.decodeRecordResult data in
     ( { model with record }, Cmd.none, (PageLoaded (RecordRoute "")) )
  | GotResult (Error e) ->
     let model = { model with record = Error (Http.string_of_error e) } in
     ( model, Cmd.none, (PageLoaded (RecordRoute "")))

  | NextResultPageLoaded (Ok _data) ->
     (model, Cmd.none, NoUpdate)
  | NextResultPageLoaded (Error _e) ->
     (* TODO handle this *)
     (model, Cmd.none, NoUpdate)

  | RecordPaginate (page, navigateCmd) ->
     ( { model with navigateCmd; nextRecord = Loading },
       Cmd.none, (LoadResultsInBackground page))

  | RecordPaginated ->
     let cmd = match model.navigateCmd with
       | Navigate (id,dir) ->
          begin
            let pagination =
              Pagination.paginateRecord ~id:id ~results ~limit:3
            in
            match pagination with
            | None -> Cmd.none
            | Some pagination -> begin
                if dir = Backward then
                  (match pagination.prev with
                   | PaginateRecordCmd id -> Router.openRoute (RecordRoute id)
                   | _ -> Cmd.none
                  )
                else
                  (match pagination.next with
                   | PaginateRecordCmd id ->
                      Router.openRoute (RecordRoute id)
                   | _ -> Cmd.none
                  )
              end
          end
       | NoNavigate -> Cmd.none
     in
     (model, cmd, NoUpdate)
  | _ -> (model, Cmd.none, NoUpdate)

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
      let imgId i =
        let id = (Util.hash recId) in
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
     let format:Types.translated = formats.((Array.length formats)-1) in
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
     let building:Types.translated = buildings.(0) in
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

let getPublishInfo (r:Types.record) =
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
          
let urlList (r:Types.record) =
  let urls =
    (match r.urls with
     | Some urls when Array.length urls > 0 -> urls |> Array.to_list
     | _ -> []) @
      (match r.onlineUrls with
       | Some urls when Array.length urls > 0 -> urls |> Array.to_list
       | _ -> [])
  in
  ul [ class' Style.recordLinks ]
    (List.map (fun (url:Types.onlineUrl) ->
         match (url.label, url.url) with
         | (Some label, Some url) ->
            li [ class' Style.recordLink ] [ a [ href url ] [ text label ] ]
         | (None, Some url) ->
            li [ class' Style.recordLink ] [ a [ href url ] [ text url ] ]
         | _ -> noNode) urls)

let finnaLink id context =
  p [] [ a [ href (Finna.getRecordLink id) ]
           [ text (Util.trans "View in Finna" context.translations) ] ]

let recordNavigation ~(record:Types.record) ~results ~context ~limit =
  let pagination =
    Pagination.paginateRecord
      ~id:record.id
      ~results
      ~limit
  in
  match pagination with
  | None -> noNode
  | Some pagination -> begin
      let totCnt = results.count in
      div [ ] [
          (
            let label = (Util.trans "Previous" context.translations) in
            match pagination.prev with
           | PaginateRecordCmd id ->
              a
                [ href (Router.routeToUrl (RecordRoute id)) ]
                [ text label ]
           | PaginatePrevCmd (page, id) ->
              a [ onClick (RecordPaginate (page, (Navigate (id, Backward))))]
                [ text label ]
           | _j -> noNode )
        ; (if totCnt > 0 then
             p [] [ text (Printf.sprintf "%d / %d" (pagination.ind+1) totCnt) ]
           else
             noNode)
        ; (
          let label = (Util.trans "Next" context.translations) in
          match pagination.next with
           | PaginateRecordCmd id ->
              a
                [ href (Router.routeToUrl (RecordRoute id)) ]
                [ text label ]
           | PaginateNextCmd (page, id) ->
              a [ onClick (RecordPaginate (page, (Navigate (id, Forward))))]
                [ text label ]
           | _ -> noNode ) ]
    end
                     
let viewRecord ~(r:Types.record) ~context ~results ~limit =
  div [ ] [
      (recordNavigation ~record:r ~results ~context ~limit)
    ; (match r.title with
       | Some title when title <> "" -> h1 [] [ text title ]
       | _ -> noNode)
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
  
let view ~model ~context ~results ~limit =
  div
    [ class' Style.recordFull ]
    [
      match model.record with
      | Loading -> statusLoading ()
      | Error e -> statusError e
      | Success r -> viewRecord ~r ~context ~results ~limit
      | _ -> Html.noNode
    ]
