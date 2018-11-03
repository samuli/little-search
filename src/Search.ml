[%%debugger.chrome]

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
  | GotFacets of (string, string Http.error) Result.t
[@@bs.deriving {accessors}]


type model = {
    searchParams: Types.searchParams;
    results: searchResultsType;
    lastSearch: string option;
    nextResult: searchResultPageType remoteData;
    visitedRecords: Types.record array;
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
    visitedRecords = [||];
    facetsOpen = false;
    facetModel = Facet.init;
    onResults = PageLoaded (SearchRoute searchParams)
  }

let getHttpCmd callback url =
  Http.send callback (Http.getString url)
               
let getSearchCmd ~(params:Types.searchParams) ~lng =
  let lng = Types.finnaLanguageCode lng in
  let params = { params with page = params.page+1 } in
  let url = Finna.getSearchUrl ~params ~lng in
  getHttpCmd gotResults url
  
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
          let items = match mode with
            | "loading" -> LoadingType items
            | "success" -> Success items
            | _ -> NotAskedType items
          in
          let facet = { facet with items } in
          Js.Dict.set facets key facet;
          facets
       | _ -> facets
     end
  | _ -> facets

let toggleFilter ~model ~filterKey ~filterVal ~mode =
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
    { model.searchParams with filters = Array.to_list filters }
  in
  let cmd = Router.openUrl (Router.routeToUrl (SearchRoute searchParams)) in
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
    
let update model context = function
  | OnSearch ->
     let searchParams = { model.searchParams with page = 0 } in
     ( { model with
         lastSearch = None;
         searchParams;
         onResults = (resultsCallback ~inBkg:false ~searchParams)
       },
       Router.openUrl (Router.routeToUrl (SearchRoute searchParams)),
       NoUpdate
     )
  | Search params ->
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
     let cmd = getSearchCmd ~params ~lng:context.language in
     ( model, cmd, NoUpdate )
  | SearchMore (page, searchInBkg) ->
     let nextResult = (LoadingType { page; results = Loading }) in
     let searchParams =
       { model.searchParams with page } in
     let (cmd, onResults) =
       let cb = resultsCallback ~inBkg:searchInBkg ~searchParams in
       if searchInBkg then
         (Cmd.msg (Search searchParams), cb)
       else
         ( Router.openUrl (Router.routeToUrl (SearchRoute searchParams)), cb)
     in
     ( { model with searchParams; nextResult; onResults }, cmd, NoUpdate )
  | OnChange lookfor ->
     let searchParams = { model.searchParams with lookfor } in
     ( { model with searchParams } , (Cmd.none), NoUpdate )
  | GotResults (Ok data) ->
     let result = Finna.decodeSearchResults data in
     let model = appendResults ~model ~newResults: result in
     let onResults =
       resultsCallback ~inBkg:false ~searchParams:model.searchParams in
     ( { model with onResults }, Cmd.none, model.onResults )
  | GotResults (Error e) ->
     let result = Error (Http.string_of_error e) in
     let model = appendResults ~model ~newResults: result in
     let onResults =
       resultsCallback ~inBkg:false ~searchParams:model.searchParams in
     ( { model with onResults }, Cmd.none, model.onResults )
  | OpenFacets -> ( model, Cmd.map facetMsg (Cmd.msg Facet.OpenFacets), NoUpdate )
  | RemoveFilter filterKey ->
     let (cmd, model) =
       toggleFilter ~model ~filterKey ~filterVal:"" ~mode:false
     in
     (model, cmd, NoUpdate)
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
          let filters =
            List.filter (fun (key, _value)
                         -> facet <> key)
              model.searchParams.filters
          in
          let params = { model.searchParams with filters } in
          let lng = Types.finnaLanguageCode context.language in
          let url = Finna.getFacetSearchUrl ~facet ~params ~lng in
          let cmd = getHttpCmd gotFacets url in
          let facets =
            updateFacet
              ~facets:model.facetModel.facets
              ~key:facet
              ~mode:"loading"
              ~items:[||]
          in
          ( { model with facetModel = { facetModel with facets} }, cmd, NoUpdate )
       | Facet.ToggleFacetItem (mode, (filterKey, filterVal)) ->
          let (cmd, model) = toggleFilter ~model ~filterKey ~filterVal ~mode in
          let model = { model with facetModel } in
          ( model, cmd, NoUpdate )
       | _ ->
          ( {model with facetModel}, (Cmd.map facetMsg subCmd), NoUpdate )
     end
  | GotFacets (Ok data) ->
     let translations = context.translations in
     let facets = match Finna.decodeFacetResults data with
       | Success (key, items) ->
          let newTranslations =
            Array.map
              (fun (f:Types.facetItem) -> (f.value, f.translated))
              items
          in
          Util.updateTranslations translations newTranslations;
          updateFacet ~facets:model.facetModel.facets ~key ~mode:"success" ~items
       | Error _e -> model.facetModel.facets
       | _ -> model.facetModel.facets
     in
     ( { model with facetModel = { model.facetModel with facets} },
       Cmd.none,
       (UpdateTranslations translations) )
  | GotFacets (Error _e) ->
     ( model, Cmd.none, NoUpdate )

let renderResultItem ~visitedRecords ~(r:Types.record) =
  let visited =
    begin try
        let _el =
          List.find (fun (el:Types.record) -> el.id = r.id)
            (Array.to_list visitedRecords)
        in
        true
      with Not_found -> false
    end in
  a [ href (Router.routeToUrl (RecordRoute r.id))        
    ]
    [ li [ id (Util.hash r.id);
           class' (Style.recordListBkg ~visited) ]
        [
          Record.authors r.authors
        ; (match r.title with
           | Some title when title <> "" -> h1 [] [ text title ]
           | _ -> noNode)
        ; Record.formats r.formats
        ; Record.publishInfo r
        ; Record.buildings r.buildings
        ]
    ]

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
  
let resultPageLoadNeighbor
      ~pageNum ~(resultPages:searchResultPageType Js.Dict.t) ~context =
  match isPageLoading ~pageNum ~resultPages with
  | Loading ->
     div
       [ class' (Style.nextPage ~loading:false) ]
       [ p [] [ text (Util.trans "Loading..." context.translations)] ]
  | NotAsked ->
    div
      [ class' (Style.nextPage ~loading:true)
      ; onClick (SearchMore (pageNum, false))
      ]
       [ text
           (Printf.sprintf "%s %d"
              (Util.trans "Page" context.translations)
              (pageNum+1))
       ]
  | _ -> noNode
  
let renderResultPage
      pageNum (searchResult:Types.searchResult) (model:model) context =
  let records = searchResult.records in
  let items =
    Array.map
      (fun r -> renderResultItem ~r ~visitedRecords:model.visitedRecords)
      records
    |> Array.to_list
  in
  div [] [
      (if pageNum > 0 then
         resultPageLoadNeighbor
           ~pageNum:(pageNum-1)
           ~resultPages:model.results.pages
           ~context
       else
         noNode)
    ; ul
        [ class' Style.searchResults]
        [ div [] items ]
    ; (if pageNum < (model.results.pageCount-1) then
         resultPageLoadNeighbor
           ~pageNum:(pageNum+1)
           ~resultPages:model.results.pages
           ~context
       else
         noNode)
    ]

let resultPage ~(page:searchResultPageType) ~model ~context =
    match (page.results, page.page) with
      | (Error e, _) -> statusError e
      | (Loading, _) -> statusLoading ()
      | (Success res, pageNum) -> renderResultPage pageNum res model context
      | _ -> Html.noNode

  
let results ~results ~model ~context =
  let pageNums = Js.Dict.keys results.pages in
  Array.sort (fun a b ->
      let a = int_of_string a in
      let b = int_of_string b in
      if a > b then 1 else -1) pageNums;
  div []
    [
      p [ class' Style.searchResultsInfo ]
        [ text (Printf.sprintf "%s: %d"
                  (Util.trans "Results" context.translations)
                  results.count)
        ]
    ; div [] 
        ((Array.map (fun pageNum->
              match Js.Dict.get results.pages pageNum with
              | Some page -> resultPage ~page ~model ~context
              | _ -> noNode) pageNums)
         |> Array.to_list)
    ]
  
let hasResults (results:Types.searchResultsType) =
  if results.count > 0 then true else false

let filters filters context =
  let items =
    List.map (fun (key, value) ->
        let label = Util.trans key context.translations in
        let value = Util.trans value context.translations in 
        p [ onClick (RemoveFilter key) ]
          [ text (Printf.sprintf "%s: %s" label value) ] ) filters
  in
  div [] items
  
let view model context =
  div
    [ ]
    [ div []
        [
          div [ class' Style.searchBoxWrapper  ]
            [ form
                [ onCB "submit" ""
                    (fun ev -> ev##preventDefault ();
                               Some(OnSearch)) ]
                [
                  input'
                    [ id "search-field"
                    ; class' Style.searchBox
                    ; type' "search"
                    ; name "lookfor"
                    ; value model.searchParams.lookfor
                    ; onInput (fun str -> (OnChange str))
                    ] []            
                ; input'
                    [ type' "submit"
                    ; value (Util.trans "Search!" context.translations)
                    ]
                    []
                ; (filters model.searchParams.filters context)
                ; ( if hasResults model.results = true then
                      a
                        [ onClick OpenFacets ]
                        [ text (Util.trans "Narrow search" context.translations) ]
                    else
                      noNode
                  )
                ]
            ]
        ]
    ; (if hasResults model.results then
         begin
           div []
             [
               results ~results:model.results ~model ~context
             ; (Facet.view
                  ~model: model.facetModel
                  ~context: context
                  ~filters:model.searchParams.filters |> App.map facetMsg)
             ]
         end
      else
        noNode)
    ]

