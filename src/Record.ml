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

let update ~model ~context = function
  | ShowRecord id ->
     let url =
       Finna.getRecordUrl ~id ~lng:(Types.finnaLanguageCode context.language)
     in
     let cmd =  Http.send gotResult (Http.getString url) in
     ( { model with nextRecord = Loading }, cmd, [NoUpdate])
  | GotResult (Ok data) ->
     let record = Finna.decodeRecordResult data in
     let visited = match record with
       | Success r -> Array.append context.visitedRecords [|r.id|]
       | _ -> context.visitedRecords
     in
     let cmds = [
         (UpdateVisitedRecords visited)
       ; (PageLoaded (RecordRoute ""))
       ] in
     ( { model with record }, Cmd.none, cmds )
  | GotResult (Error e) ->
     let model = { model with record = Error (Http.string_of_error e) } in
     ( model, Cmd.none, [PageLoaded (RecordRoute "")])
  | NextResultPageLoaded (Ok _data) ->
     (model, Cmd.none, [NoUpdate] )
  | NextResultPageLoaded (Error _e) ->
     (* TODO handle this *)
     (model, Cmd.none, [NoUpdate] )

  | RecordPaginate (page, navigateCmd) ->
     ( { model with navigateCmd; nextRecord = Loading },
       Cmd.none, [LoadResultsInBackground page])

  | RecordPaginated ->
     let cmd = match model.navigateCmd with
       | Navigate (id,dir) ->
          begin
            let pagination =
              Pagination.paginateRecord
                ~id:id
                ~limit:context.resultLimit
                ~resultCount:context.numOfResults
                ~recordIds:context.recordIds
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
     (model, cmd, [NoUpdate] )
  | CloseRecord ->
     (model, Cmd.none, [BackToSearch] )
  | _ -> (model, Cmd.none, [NoUpdate] )

let images recId imgs =
  (match imgs with
   | Some images when (Array.length images) = 0 -> noNode
   | Some images ->
      let item i path imgId =
        let path = Finna.baseUrl ^ path in
        let attrs =
          [ id imgId; class' (Style.recordImage ~loading:true)
            ; (onCB "load" "" (fun e ->
                   (match Js.Undefined.toOption e##target with
                    | Some target -> target##setAttribute "class" "loaded";
                    | _ -> ());
                   None))            
          ] @
            (* load first images immediately and the rest with inView.js *)
            (if i < 4 then [ src path; noProp ] else [ noProp; href path ])
        in
        
        li [ class' Style.recordImageContainer]
          [
            img ~unique:imgId attrs []
          ]
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
  p []
    [ a [ href (Finna.getRecordLink id)
        ; class' Style.recordFinnaLink ]
        [ text (Util.trans "View in Finna" context.translations) ] ]

let recordNavigation ~(record:Types.record) ~(context:context) =
  let resultCount = context.numOfResults in
  let pagination =
    Pagination.paginateRecord
      ~id:record.id
      ~recordIds:context.recordIds
      ~resultCount
      ~limit:context.resultLimit
  in

  let renderPagination pagination =
    let loadBkgProp = (Vdom.prop "load-in-bkg" "1") in
    div [] [
        (* Set a dummy attribute for background loading paginate links so that the vdom notices that they differ from normal paginate links... *)
        (let label = (Util.trans "Previous" context.translations) in
         match pagination.prev with
         | PaginateRecordCmd recId ->
            a [ class' (Style.arrowIcon Style.ArrowLeft)
              ; href (Router.routeToUrl (RecordRoute recId))
              ; title label ] []
         | PaginatePrevCmd (page, recId) ->
            a [ class' (Style.arrowIcon Style.ArrowLeft)
              ; loadBkgProp
              ; onClick (RecordPaginate (page, (Navigate (recId, Backward))))
              ; title label ] []
         | _j -> noNode)
      ; (if resultCount > 0 then
           p [ class' Style.paginateInfo]
             [ text (Printf.sprintf "%d / %d" (pagination.ind+1) resultCount) ]
         else
           noNode)
      ; (
        let label = (Util.trans "Next" context.translations) in
        match pagination.next with
        | PaginateRecordCmd recId ->
           a [ class' (Style.arrowIcon Style.ArrowRight)
             ; href (Router.routeToUrl (RecordRoute recId))
             ; title label ] []
        | PaginateNextCmd (page, recId) ->
           a [ class' (Style.arrowIcon Style.ArrowRight)
             ; loadBkgProp
             ; onClick (RecordPaginate (page, (Navigate (recId, Forward))))
             ; title label
             ] []
        | _ -> noNode)
      ]
  in
  
  div [ class' Style.paginationContainer]
    [
      div [] [
          a [ class' Style.closeRecordIcon
            ; onClick CloseRecord ] []
        ]
    ; (match pagination with
       | None -> noNode
       | Some pagination -> renderPagination pagination)
    ]
                     
let viewRecord ~(r:Types.record) ~context =
  div [] [
      (recordNavigation ~record:r ~context)
    ; div [ class' Style.recordContent ] [
          (match r.title with
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
    ]
  
let view ~model ~context =
  div
    []
    [
      (match model.record with
       | Loading -> statusLoading ~context
       | Error e -> statusError e
       | Success r -> viewRecord ~r ~context
       | _ -> Html.noNode)
    ] 
