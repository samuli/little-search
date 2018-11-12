open Types

open View
   
open Tea
open Tea.Html

type msg =
  | OnSearch
  | Search of Types.searchParams
  | SearchMore of (int * bool)
  | OnChange of string
  | GotResults of (string, string Http.error) Result.t
  | RemoveFilter of (string)
  | OpenFacets
  | FacetMsg of Facet.msg
  | GotFacets of (string * string)
  | GotFacetsError of string
  | IgnoreRecordMsg of Record.msg
[@@bs.deriving {accessors}]


type model = {
    searchParams: Types.searchParams;
    results: searchResultsType;
    lastSearch: string option;
    nextResult: searchResultPageType remoteData;
    facetsOpen: bool;
    facetModel: Facet.model;
    onResults: contextUpdate;
  }


let initResults _ =
  {
    count = 0;
    pageCount = 0;
    pages = Js.Dict.empty();
  }


let init =
  let searchParams =
    {
      lookfor = "";
      page = 0;
      limit = 10;
      filters = [];
    }
  in
  {
    searchParams;
    lastSearch = None;
    results = initResults ();
    nextResult = NotAsked;
    facetsOpen = false;
    facetModel = Facet.init;
    onResults = PageLoaded (SearchRoute searchParams)
  }

let getHttpCmd callback url =
  Http.send callback (Http.getString url)
               
let getSearchCmd ~settings ~(params:Types.searchParams) ~lng =
  match Util.getApiUrl settings with
  | Some apiUrl ->
     let lng = Types.finnaLanguageCode lng in
     let params = { params with page = params.page+1 } in
     let url = Finna.getSearchUrl ~apiUrl ~params ~lng in
     getHttpCmd gotResults url
  | _ -> Cmd.none
  
let appendResults ~model ~(newResults:Types.searchResult remoteData) =
  let page = match model.nextResult with
    | LoadingType page -> page.page
    | _ -> model.searchParams.page
  in

  let (resultPage, count)  = match newResults with
    | Success res -> begin
        let count = res.resultCount in
        let resultPage = { page; results = Success res } in
        (resultPage, count)
      end
    | _ -> ({ page; results = Error ""}, 0)
  in

  let pages = model.results.pages in
  Js.Dict.set pages (string_of_int page) resultPage;
  let pageCount =
    ceil ((float_of_int count) /. (float_of_int model.searchParams.limit)) +. 0.5
    |> int_of_float in

  let results = { pages; count; pageCount } in
  
  { model with
    results;
    nextResult = NotAsked;
    lastSearch = Some model.searchParams.lookfor
  }

let updateFacet ~facets ~key ~mode ~items =
  match Js.Dict.get facets key with
  | Some (facet:Facet.facet) ->
     begin
       match facet.items with
       | NotAskedType _t | LoadingType _t | Success _t ->
          let (items,count) = match mode with
            | "loading" -> (LoadingType items, None)
            | "success" ->
               let count =
                 Array.fold_left
                   (fun cnt (facet:Types.facetItem) -> cnt+facet.count) 0 items
               in
               (Success items, Some count)
            | _ -> (NotAskedType items, None)
          in
          let facet = { facet with items; count } in
          Js.Dict.set facets key facet;
          facets
       | _ -> facets
     end
  | _ -> facets

let toggleFilter ~context ~model ~filterKey ~filterVal ~mode =
  let filters = model.searchParams.filters in
  let filters = 
    if mode then
      let filters =
        List.filter (fun (key, _value) ->
            key <> filterKey) filters |> Array.of_list
      in
      Array.append filters [| (filterKey, filterVal) |]
    else
      List.filter (fun (fKey, fVal) ->
          (fVal <> filterVal && fKey <> filterKey))
        filters
      |> Array.of_list
  in
  let searchParams =
    { model.searchParams with filters = Array.to_list filters; page = 0 }
  in
  let cmd = Router.openUrl
              ~appPath:context.appPath
              ~url:(Router.routeToUrl (SearchRoute searchParams)) in
  let model = { model with searchParams;
                           lastSearch = None;
                           nextResult = Loading }
  in
  (cmd, model)

let resultsCallback ~inBkg ~searchParams =
  if inBkg then
    GotResultsInBackground
  else
    (PageLoaded (SearchRoute searchParams))
  
let isPageLoading ~pageNum ~(resultPages:searchResultPageType Js.Dict.t) =
  match Js.Dict.get resultPages (string_of_int pageNum) with
  | Some page -> begin
      match page.results with
      | LoadingType _ -> Loading
      | Success p -> Success p
      | NotAsked -> NotAsked
      | _ -> Error ""
    end
  | _ -> NotAsked

let isLoading ~model =
  match model.nextResult with
  | LoadingType _ -> true
  | _ -> false

let update model context = function
  | OnSearch ->
     let searchParams = { model.searchParams with page = 0 } in
     ( { model with
         lastSearch = None;
         searchParams;
         onResults = (resultsCallback ~inBkg:false ~searchParams)
       },
       Router.openUrl
         ~appPath:context.appPath
         ~url:(Router.routeToUrl (SearchRoute searchParams)),
       [NoUpdate]
     )
  | Search params ->
     if isLoading ~model = true then
       ( model, Cmd.none, [NoUpdate] )
     else
       let newSearch =
         match model.lastSearch with
         | None -> true
         | Some query -> not (query == params.lookfor) in
       let page = params.page in       
       let nextResult = { page; results = Loading } in
       let (results, onResults) =
         if newSearch then
           (initResults (), (resultsCallback ~inBkg:false ~searchParams:params))
         else
           let pages = model.results.pages in
           Js.Dict.set pages (string_of_int page) nextResult;
           ( { model.results with pages }, model.onResults )
       in
       let nextResult = LoadingType nextResult in
       let model =
         { model with results;
                      nextResult;
                      onResults;
                      searchParams = params; }
       in
       let cmd = getSearchCmd
                   ~settings:context.settings
                   ~params
                   ~lng:context.language
       in
       ( model, cmd, [NoUpdate] )
       
  | SearchMore (page, searchInBkg) ->
     begin
       match (model.nextResult) with
       | (LoadingType _) -> (model, Cmd.none, [NoUpdate])
       | _ ->
          let searchParams =
            { model.searchParams with page } in
          let (cmd, onResults) =
            let cb = resultsCallback ~inBkg:searchInBkg ~searchParams in
            if searchInBkg then
              (Cmd.msg (Search searchParams), cb)
            else
              ( Router.openUrl
                  ~appPath:context.appPath
                  ~url:(Router.routeToUrl (SearchRoute searchParams)), cb)
          in
          ( { model with searchParams; onResults; }, cmd, [NoUpdate] )
     end
       
  | OnChange lookfor ->
     let searchParams = { model.searchParams with lookfor } in
     ( { model with searchParams } , (Cmd.none), [NoUpdate] )
  | GotResults (Ok data) ->
     let result = Finna.decodeSearchResults data in
     let model = appendResults ~model ~newResults: result in

     let (recIds, _pageNums) = Pagination.recordPages model.results in
     let updateResultInfo =
       UpdateResultInfo (
           model.results.count, recIds, model.searchParams.limit
         )
     in
     let onResults =
       resultsCallback ~inBkg:false ~searchParams:model.searchParams in
     ( { model with onResults }, Cmd.none, [updateResultInfo; model.onResults] )
  | GotResults (Error e) ->
     let result = Error (Http.string_of_error e) in
     let model = appendResults ~model ~newResults: result in
     let onResults =
       resultsCallback ~inBkg:false ~searchParams:model.searchParams in
     ( { model with onResults }, Cmd.none, [model.onResults] )

  | OpenFacets ->
     ( model, Cmd.map facetMsg (Cmd.msg Facet.OpenFacets), [NoUpdate] )

  | RemoveFilter filterKey ->
     let (cmd, model) =
       toggleFilter ~context ~model ~filterKey ~filterVal:"" ~mode:false
     in
     (model, cmd, [NoUpdate])
  | FacetMsg subMsg ->
     let lookfor = match model.lastSearch with
       | Some search -> search
       | _ -> model.searchParams.lookfor
     in          
     let (facetModel, subCmd) =
       (Facet.update
          ~model:model.facetModel
          ~lookfor
          ~filters:model.searchParams.filters subMsg)
     in 
     begin
       match subMsg with
       | Facet.GetFacets facet ->
          let (model, cmd) = match Util.getApiUrl context.settings with
            | Some apiUrl ->
               let filters =
                 List.filter (fun (key, _value)
                              -> facet <> key)
                   model.searchParams.filters
               in
               let params = { model.searchParams with filters } in
               let lng = Types.finnaLanguageCode context.language in

               let url =
                 Finna.getFacetSearchUrl
                   ~apiUrl ~facet ~params ~lng
               in
               let cb = fun res ->
                 match res with
                 | (Tea.Result.Ok data) -> gotFacets (data, facet)
                 | (Tea.Result.Error _) -> gotFacetsError facet
               in
               
               let cmd = getHttpCmd cb url in
               let facets =
                 updateFacet
                   ~facets:model.facetModel.facets
                   ~key:facet
              ~mode:"loading"
              ~items:[||]
               in
               ( { model with facetModel = { facetModel with facets} }, cmd )
            | _ -> (model, Cmd.none)
          in
          ( model, cmd, [NoUpdate] )
               
       | Facet.ToggleFacetItem (mode, (filterKey, filterVal)) ->
          let (cmd, model) =
            toggleFilter ~context ~model ~filterKey ~filterVal ~mode
          in
          let model = { model with facetModel } in
          ( model, cmd, [NoUpdate] )
       | _ ->
          ( {model with facetModel}, (Cmd.map facetMsg subCmd), [NoUpdate] )
     end

  | GotFacets (data, key) ->
     let translations = context.translations in
     let facets = match Finna.decodeFacetResults data with
       | Success (key, items) ->
          let newTranslations =
            Array.map
              (fun (f:Types.facetItem) -> (f.value, f.translated))
              items
          in
          Util.updateTranslations translations newTranslations;
          updateFacet
            ~facets:model.facetModel.facets ~key ~mode:"success" ~items
       | Error _e ->
          updateFacet
            ~facets:model.facetModel.facets ~key ~mode:"success" ~items:[||]
       | _ -> model.facetModel.facets
     in
     ( { model with facetModel = { model.facetModel with facets} },
       Cmd.none,
       [(UpdateTranslations translations)] )

  | GotFacetsError _facetKey ->
     ( model, Cmd.none, [NoUpdate] )
    
  | _ -> (model, Cmd.none, [NoUpdate] )

let renderResultItem ~(r:Types.record) ~visitedRecords =
  let lastVisited =
    let len = Array.length visitedRecords in
    if len = 0 then
      false
    else
      Array.get visitedRecords (len-1) = r.id
  in
  let visited =
    begin try
        let _el =
          List.find (fun id -> id = r.id)
            (Array.to_list visitedRecords)
        in
        true
      with Not_found -> false
    end in
  a [ href (Router.routeToUrl (RecordRoute r.id))        
    ]
    [ li [ id (Util.hash r.id);
           class' (Style.recordListBkg ~visited ~lastVisited) ]
        [
          Record.authors r.authors
        ; (match r.title with
           | Some title when title <> "" -> h1 [] [ text title ]
           | _ -> noNode)
        ; Record.formats ~formats:r.formats ~link:false |> App.map ignoreRecordMsg
        ; Record.publishInfo r
        ; Record.buildings r.buildings
        ]
    ]

  
let resultPageLoadNeighbor
      ~model
      ~pageNum ~(resultPages:searchResultPageType Js.Dict.t) ~context ~dir =

  let pageLoading = isPageLoading ~pageNum ~resultPages in
  match pageLoading with
  | Loading | NotAsked ->
     begin
       let arrow = if dir = Backward then Style.ArrowUp else Style.ArrowDown in
       let isLoading = isLoading ~model in
       div
         [ class' (Style.nextPage ~loading:isLoading)
         ; (if isLoading then noProp else onClick (SearchMore (pageNum, false)))
         ]
         [
           p []
             [ span [ class' Style.nextPageLabel ]
                 [ text (Printf.sprintf "%s %d / %d"
                           (Util.trans "Page" context.translations)
                           (pageNum+1) model.results.pageCount) ]
             ; span [ class' (Style.arrowIcon arrow) ] []
             ]
         ]
     end
     | _ -> noNode
  
let renderResultPage
      pageNum (searchResult:Types.searchResult) (model:model) context =
  let records = searchResult.records in
  let items =
    Array.map
      (fun r -> renderResultItem ~r ~visitedRecords:context.visitedRecords)
      records
    |> Array.to_list
  in
  div [] [
      (if pageNum > 0 then
         resultPageLoadNeighbor
           ~model
           ~pageNum:(pageNum-1)
           ~resultPages:model.results.pages
           ~context
           ~dir:Backward
       else
         noNode)
    ; ul
        [ class' Style.searchResults]
        [ div [] items ]
    ; (if pageNum < (model.results.pageCount-1) then
         resultPageLoadNeighbor
           ~model
           ~pageNum:(pageNum+1)
           ~resultPages:model.results.pages
           ~context
           ~dir:Forward
       else
         noNode)
    ]

let resultPage ~(page:searchResultPageType) ~model ~context =
    match (page.results, page.page) with
    | (Error e, _) -> statusError e
    | (Loading, _) ->
       div [ class' (Style.nextPage ~loading:true) ]
         [
           p []
             [ span [ class' Style.nextPageLabel ]
                 [ text (Util.trans "Loading..." context.translations) ]
             ; span [ class' (Style.spinnerIcon) ] []
             ]
         ]
    | (Success res, pageNum) -> renderResultPage pageNum res model context
    | _ -> Html.noNode


let hasResults (results:Types.searchResultsType) =
  if results.count > 0 then true else false

let results ~results ~model ~context =
  let pageNums = Js.Dict.keys results.pages in
  Array.sort (fun a b ->
      let a = int_of_string a in
      let b = int_of_string b in
      if a > b then 1 else -1) pageNums;
  div []
    [
      (if (not (isLoading ~model)) || hasResults results then
         h3 [ class' Style.searchResultsInfo ]
           [ text (Printf.sprintf "%s: %s"
                     (Util.trans "Results" context.translations)
                     (Util.toLocaleString results.count))
           ]
       else
         noNode
      )
    ; div [] 
        ((Array.map (fun pageNum->
              match Js.Dict.get results.pages pageNum with
              | Some page -> resultPage ~page ~model ~context
              | _ -> noNode) pageNums)
         |> Array.to_list)
    ]
  

let openFilters ~results ~context =
  let results = hasResults results in
  let label = (Util.trans "Narrow search" context.translations) in
  div [ class' (Style.openFacets ~active:results)
      ; (if results = true then onClick OpenFacets else noProp) ]
    [
      a [ class' Style.facetsIcon
        ; title label ]
        [
          p [ class' Style.facetsIconLabel ]
            [ text label ]
        ]
    ]

let filters filters context =
  let items =
    List.map (fun (key, value) ->
        let type' =
          if key == "online_boolean" then
            FacetBoolean else FacetNormal
        in
        let label = Util.trans key context.translations in
        let value =
          Facet.getFacetLabel
            ~translations:context.translations
            ~key
            ~value
            ~type'
        in

        div [
            onClick (RemoveFilter key)
          ; class' Style.removeFilter
          ]
          [
            div [ class' Style.removeFilterIcon ] []
          ; div [ class' Style.removeFilterLabel ]
              [
                p [ class' Style.filterType ]
                  [ text (Printf.sprintf "%s:" label) ]
              ; p [ class' Style.filterLabel]
                  [ text value ]
              ]
          ]
      ) filters
  in
  div [ class' Style.filterContainer ] items

let blurSearchfield () =
  [%bs.raw "document.getElementById(\"search-field\").blur() "]

let searchField ~lookfor ~context ~disable =
  div [] [
      div
        [ class' (Style.searchBoxSubmit ~active:(not disable))
        ; title (Util.trans "Search!" context.translations)
        ; (if disable then noProp else onClick OnSearch)
        ] []
        
      (* input'
       *   [ type' "submit"
       *   ; class' (Style.searchBoxSubmit ~active:(not disable))
       *   ; Attributes.disabled disable
       *   ; title (Util.trans "Search!" context.translations)
       *   ]
       *   [] *)
        
    ; div [ class' Style.searchBoxInputWrapper ] [
          input'
            [ id "search-field"
            ; class' Style.searchBox
            ; type' "search"
            ; name "lookfor"
            ; value lookfor
            ; onInput (fun str -> (OnChange str))
            ] []
        ]
    ]
  
let view model context ~onMainPage =
  div
    [ ]
    [ 
      
      div [ class' Style.searchBoxWrapper] [
          form
            [ onCB "submit" ""
                (fun ev ->
                  ev##preventDefault ();
                  let () = blurSearchfield () in
                  Some(OnSearch))
            ]
            [
              (searchField
                 ~lookfor:model.searchParams.lookfor
                 ~context
                 ~disable:(isLoading ~model))
            ]
        ]
    
    ; (if (not onMainPage) then
         
         div [ class' Style.filterTools ]
           [
             (openFilters ~results:model.results ~context)
           ; (filters model.searchParams.filters context)
           ]
       else
         noNode)
    
    
    ; (if (not onMainPage) then
         div []
           [
             results ~results:model.results ~model ~context
           ; (Facet.view
                ~model: model.facetModel
                ~context: context
                ~filters:model.searchParams.filters |> App.map facetMsg)
           ]
       else
         noNode)
    
    ]

