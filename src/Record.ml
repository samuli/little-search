open Types
open View
open Tea
open Tea.Html


type msg =
  | AddColumn
  | RemoveColumn
  | ShowRecord of string
  | RecordPaginate of (resultpageNum * navigateCmd)
  | RecordPaginated
  | NextResultPageLoaded of (int, string) Result.t
  | LoadSearchResults of int
  | GotResult of (string, string Http.error) Result.t
  | CloseRecord
  | SearchLinkClick of searchterm
  | FacetLinkClick of searchParam
[@@bs.deriving {accessors}]

type model = {
    gridColumns: int;
    record: Types.record remoteData;
    nextRecord: Types.record remoteData;
    navigateCmd: navigateCmd;
  }

let init =
  {
    gridColumns = 3;
    record = NotAsked;
    nextRecord = NotAsked;
    navigateCmd = NoNavigate;
  }

let update ~model ~context = function
  | AddColumn ->
     ( { model with gridColumns = model.gridColumns+1 }, Cmd.none, [NoUpdate] )
  | RemoveColumn ->
     let gridColumns = max 0 (model.gridColumns-1) in
     ( { model with gridColumns }, Cmd.none, [NoUpdate] )
  | ShowRecord id ->
     let (model, cmd) = match Util.getApiUrl context.settings with
       | Some apiUrl ->
          let url =
            Finna.getRecordUrl
              ~apiUrl
              ~id
              ~lng:(Types.finnaLanguageCode context.language)
          in
          let cmd =  Http.send gotResult (Http.getString url) in
          ( { model with nextRecord = Loading }, cmd)
       | _ -> (model, Cmd.none)
     in
     ( model, cmd, [NoUpdate] )
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
                   | PaginateRecordCmd id ->
                      Router.openRoute
                        ~appPath:context.appPath ~route:(RecordRoute id)
                   | _ -> Cmd.none
                  )
                else
                  (match pagination.next with
                   | PaginateRecordCmd id ->
                      Router.openRoute
                        ~appPath:context.appPath ~route:(RecordRoute id)
                   | _ -> Cmd.none
                  )
              end
          end
       | NoNavigate -> Cmd.none
     in
     (model, cmd, [NoUpdate] )
  | CloseRecord ->
     (model, Cmd.none, [BackToSearch] )

  | SearchLinkClick lookfor ->
     ( model, Cmd.none, [NewSearch (Some lookfor, None)] )

  | FacetLinkClick (key,value) ->
     ( model, Cmd.none, [NewSearch (None, Some (key,value))] )

  | _ -> (model, Cmd.none, [NoUpdate] )

let images recId imgs ~columns =
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

      (* let rec divide limit pages page items =
       *   match items with
       *   | [] -> List.append pages page
       *   | hd :: tl ->
       *      let cnt = List.length page in
       *      if cnt = limit then
       *        let pages = List.append pages page in
       *        let page = [hd] in
       *        divide limit pages page tl
       *      else
       *        let page = List.append page [hd] in
       *        divide limit pages page tl
       * in
       * 
       * let pages = divide 10 [] [] (Array.to_list images) in
       * Js.log ("pages", (Array.of_list pages).(0)); *)
      
      ul [ class' (Style.recordImages columns) ]
             (Array.to_list images)
   | None -> noNode)


let getFormats formats =
  match formats with
  | Some formats when Array.length formats > 0 ->
     let format:Types.translated = formats.((Array.length formats)-1) in
     Some (format.value,format.translated)
  | _ -> None

let renderFormat ~value ~translated ~link =
  span
    [ class' Style.recordFormat
    (* ; if (link = true) then onClick (FacetLinkClick ("format", value)) else noProp *)
    ]
    [ text translated ]

let formats ~formats ~link =
  match getFormats formats with
  | Some (value,translated) -> renderFormat ~value ~translated ~link
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
  let publisher = match r.publishers with
    | Some publishers when Array.length publishers > 0 ->
       Some publishers.(0)
    | _ -> None
  in
  let year = match r.publicationDates with
    | Some dates when Array.length dates > 0 ->
       Some dates.(0)
    | _ -> None
  in
  match (publisher, year) with
  | (Some pub, Some year) -> Some (Printf.sprintf "%s %s" pub year)
  | (Some pub, _) -> Some pub
  | (_, Some year) -> Some year
  | _ -> None

let publishInfo r =
  match getPublishInfo r with
  | Some info -> span [ class' Style.recordPublisher ] [ text info ]
  | _ -> noNode

let recordField ~css ~field ~value =
  p [ class' css ] [
      span
        [ class' Style.recordFieldHeader ]
        [ text (Printf.sprintf "%s: " field) ]
    ; span
        [ class' Style.recordFieldText ]
        [ text value ]
    ]

let maybeFirst arr =
  match arr with
  | Some arr when Array.length arr > 0 -> Some arr.(0)
  | _ -> None

let renderRecordRow value =
  p [ class' Style.recordFieldRow ] [ text value ]
  
let recordRow value =
  match value with
  | Some value -> renderRecordRow value
  | None -> noNode

let recordRows value =
  div [] 
    (match value with
    | Some value ->
       List.map (fun r -> renderRecordRow r) (Array.to_list value)
    | None -> [noNode])

let isbn r =
  let (field, num) = match (r.isbn, r.issn) with
    | (Some isbn, _) -> ("ISBN", Some isbn)
    | (_, Some issn) -> ("ISSN", Some issn)
    | (_,_) -> ("", None)
  in
  match num with
  | Some num -> recordField ~css:Style.recordIsbn ~field ~value:num
  | None -> noNode

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
  if List.length urls > 0 then
    ul [ class' Style.recordLinks ]
      (List.map (fun (url:Types.onlineUrl) ->
           match (url.label, url.url) with
           | (Some label, Some url) ->
              li
                [ class' Style.recordLink ]
                [ a
                    [ class' Style.textLink; href url ]
                    [ text label ] ]
           | (None, Some url) ->
              li
                [ class' Style.recordLink ]
                [ a [ class' Style.textLink; href url ]
                    [ text url ] ]
           | _ -> noNode) urls)
  else
    noNode

let searchLink ~lookfor ~label ~exact =
  let term =
    if exact then
      Printf.sprintf "\"%s\"" lookfor
    else lookfor
  in
  a [ class' Style.recordSearchLink
    ; onClick (SearchLinkClick term) ]
    [ text label ]
  
let searchLinkList ~title ~list =
  match list with
  | Some items when Array.length items > 0 ->
     div [ class' Style.recordSearchLinksContainer ] [
         h3 [] [ text title ]
       ; ul [ class' Style.recordSearchLinkList ]
           ((Array.map (fun text ->
                 let link = searchLink ~lookfor:text ~label:text ~exact:true in
                 li [] [ link ]) items) |> Array.to_list)
       ]
  | _ -> noNode
  
let finnaLink id context =
  match Util.getSiteUrl context.settings with
  | Some siteUrl ->
     p [ class' Style.recordFinnaLink ]
       [ a
           [ href (Finna.getRecordLink ~siteUrl ~id)
           ; class' Style.textLink ]
           [ text (Util.trans "View in Finna" context.translations) ] ]
  | _ -> noNode
       
let recordNavigation ~(record:Types.record) ~(context:context) ~columns =
  let resultCount = context.numOfResults in
  let pagination =
    Pagination.paginateRecord
      ~id:record.id
      ~recordIds:context.recordIds
      ~resultCount
      ~limit:context.resultLimit
  in

  let renderPagination pagination =
    let label = (Util.trans "Previous" context.translations) in
    let el = match pagination.prev with
    | PaginateRecordCmd recId ->
       a [ class' (Style.paginateButton Style.ArrowLeft)
         ; href (Router.routeToUrl (RecordRoute recId))
         ; title label ] []
    | PaginatePrevCmd (page, recId) ->
       a [ class' (Style.paginateButton Style.ArrowLeft)
         ; onClick (RecordPaginate (page, (Navigate (recId, Backward))))
         ; title label ] []
    | _j -> noNode
    in
    
    let el = List.append [el] [(if resultCount > 0 then
       p [ class' Style.paginateInfo]
         [ text (Printf.sprintf "%d / %d" (pagination.ind+1) resultCount) ]
     else
       noNode)]
    in

    let label = (Util.trans "Next" context.translations) in

    let el = List.append el [(match pagination.next with
    | PaginateRecordCmd recId ->
         a [ class' (Style.paginateButton Style.ArrowRight)
           ; href (Router.routeToUrl (RecordRoute recId))
           ; title label ] []
    | PaginateNextCmd (page, recId) ->
       a [ class' (Style.paginateButton Style.ArrowRight)
         ; onClick (RecordPaginate (page, (Navigate (recId, Forward))))
         ; title label
         ] []
    | _ -> noNode)]
    in
    
    el
    
  in  
    
  div [ class' Style.paginationContainer]
    [
      div []        
        (List.append (match pagination with
                      | None -> [noNode]
                      | Some pagination -> renderPagination pagination)
           [ div []
               [
               a [ class' Style.closeRecordIcon
                 ; onClick CloseRecord ] []

               ; div [ class' Style.recordImageGridUi] [
                     div [ class' Style.zoomInIcon
                         ; onClick RemoveColumn ] []
                   ; div [ class' Style.zoomOutIcon
                         ; onClick AddColumn ] []
                   ]
               ]
           ]
        )
    ]
                     
let viewRecord ~(r:Types.record) ~context ~model =
  let subjects = match r.subjects with
    | Some subjects -> Some (Array.map (fun el -> el.(0)) subjects)
    | _ -> None
  in
  div [] [
      (recordNavigation ~record:r ~context ~columns:model.gridColumns)
    ; div [ class' Style.recordContent ] [
          (match r.title with
           | Some title when title <> "" -> h1 [] [ text title ]
           | _ -> noNode)
        ; summary r.summary
        ; authors r.authors
        ; publishInfo r
        ; isbn r
        ; div [ class' Style.recordRow ] [
              formats ~formats:r.formats ~link:true
            ; buildings r.buildings
            ]
        ; urlList r
        ; recordRows r.measurements
        ; images r.id r.images ~columns:model.gridColumns
        ; searchLinkList
            ~title:(Util.trans "Subjects" context.translations)
            ~list:subjects
        ; searchLinkList
            ~title:(Util.trans "Genres" context.translations)
            ~list:r.genres
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
       | Success r -> viewRecord ~r ~context ~model
       | _ -> Html.noNode)
    ] 
