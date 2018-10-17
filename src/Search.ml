open Types
open Finna

open View
   
open Tea
open Tea.Html

type msg =
  | OnSearch
  | Search
  | SearchMore
  | OnChange of string
  | GotResults of (string, string Http.error) Result.t
  | ShowRecord of Finna.record
  | PageLoaded
  | OpenFacets
  | FacetMsg of Facet.msg
  | GotFacets of (string, string Http.error) Result.t                   
[@@bs.deriving {accessors}]

type model = {
    results: Finna.searchResult remoteData;
    lookfor: string;
    page: int;
    limit: int;
    filters: Finna.filter array;
    lastSearch: string option;
    nextResult: Finna.searchResult remoteData;
    visitedRecords: Finna.record array;
    facetsOpen: bool;
    facetModel: Facet.model
  }

let init =
  {
    lookfor = "";
    page = 1;
    limit = 30;
    filters = [||];
    lastSearch = None;
    results = NotAsked;
    nextResult = NotAsked;
    visitedRecords = [||];
    facetsOpen = false;
    facetModel = Facet.init
  }

let getHttpCmd callback url =
  Http.send callback (Http.getString url)
               
let getSearchCmd ~lookfor ~page ~limit ~filters =
  let url = Finna.getSearchUrl ~lookfor ~page ~limit ~filters in
  getHttpCmd gotResults url
  
let appendResults ~model ~newResults =
  let allResults = match (model.results, newResults, model.lastSearch) with
    | (NotAsked, _, _) | (_, _, None) -> newResults
    | (Success result, Success newRes, _) ->
       let records = (List.append (Array.to_list result.records) (Array.to_list newRes.records)) |> Array.of_list in
       Success { result with records }
    | (t, _, _) -> t 
  in
  { model with
    results = allResults;
    nextResult = NotAsked;
    lastSearch = Some model.lookfor
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

let update model = function
  | OnSearch ->
     ( { model with lastSearch = None; filters = [||] },
       Router.openUrl (Router.routeToUrl (Search model.lookfor)))
  | Search ->
     let newSearch =
       match model.lastSearch with
       | None -> true
       | Some query -> not (query == model.lookfor) in
     if newSearch then
       let cmd =
         getSearchCmd ~lookfor:model.lookfor ~page:1 ~limit:model.limit ~filters:model.filters in
       ( { model with nextResult = Loading }, cmd )
     else
       ( model, Cmd.msg pageLoaded )
  | SearchMore ->
     let page = model.page+1 in
     let cmd =
       getSearchCmd ~lookfor:model.lookfor ~page ~limit:model.limit ~filters:model.filters in
     ( { model with
         nextResult = Loading;
         lastSearch = Some model.lookfor;
         page
       }, cmd )
  | OnChange lookfor -> ( { model with lookfor } , (Cmd.none) )
  | GotResults (Ok data) ->
     let result = Finna.decodeSearchResults data in
     let model = appendResults ~model ~newResults: result in
     ( model, Cmd.msg pageLoaded )
  | GotResults (Error e) ->
     let result = Error (Http.string_of_error e) in
     let model = appendResults ~model ~newResults: result in
     ( model, Cmd.none )
  | ShowRecord r ->
     let cmd = Router.openRoute (Record r.id) in
     let visitedRecords = Array.append model.visitedRecords [| r |] in
     ( { model with visitedRecords }, cmd )
  | PageLoaded -> ( model, Cmd.none )
  | OpenFacets -> ( model, Cmd.map facetMsg (Cmd.msg Facet.OpenFacets) )
  | FacetMsg subMsg ->
     let lookfor = match model.lastSearch with
       | Some search -> search
            | _ -> model.lookfor
     in          
     let (facetModel, subCmd) = (Facet.update ~model:model.facetModel ~lookfor ~filters:model.filters subMsg) in 
     begin
       match subMsg with
       | Facet.GetFacets facet ->
          let filters = List.filter (fun f -> f.key <> facet) (Array.to_list model.filters) |> Array.of_list in
          let url = Finna.getFacetSearchUrl ~lookfor ~page:model.page ~facet ~filters in
          let cmd = getHttpCmd gotFacets url in
          let facets = updateFacet ~facets:model.facetModel.facets ~key:facet ~mode:"loading" ~items:[||] in
          ( { model with facetModel = { facetModel with facets} }, cmd )
       | Facet.ToggleFacet (mode, filter) ->
          let filters = 
            if mode then
              let filters =
                List.filter (fun f -> f.key <> filter.key) (Array.to_list model.filters) |> Array.of_list
              in
              Array.append filters [| filter |]
            else
              List.filter (fun (f:Finna.filter) ->
                  (f.value <> filter.value && f.key <> filter.key))
                (Array.to_list model.filters)
              |> Array.of_list
          in
          ( { model with filters; facetModel; lastSearch = None }, Cmd.msg search )
       | _ ->
          ( {model with facetModel}, (Cmd.map facetMsg subCmd) )
     end
  | GotFacets (Ok data) ->
     let facets = match Finna.decodeFacetResults data with
       | Success (key, items) ->
          updateFacet ~facets:model.facetModel.facets ~key ~mode:"success" ~items
       | Error _e -> model.facetModel.facets
       | _ -> model.facetModel.facets
     in
     ( { model with facetModel = { model.facetModel with facets} }, Cmd.none )
  | GotFacets (Error _e) ->
     ( model, Cmd.none )

     
let renderResultItem visitedRecords r =
  let visited =
    begin try
        let _el =
          List.find (fun el -> el.id = r.id) (Array.to_list visitedRecords) in
        true
      with Not_found -> false
    end in
  a [ onClick (ShowRecord r)
    ]
    [ li [ class' (Style.recordListBkg ~visited) ]
        [
          Record.authors r.authors
        ; h1 [] [ text r.title ]
        ; Record.formats r.formats
        ; Record.publishInfo r
        ; Record.buildings r.buildings
        ]
    ]

let renderResults (result:Finna.searchResult) model =
  let pages =
    floor ((float_of_int result.resultCount) /. (float_of_int model.limit)) +. 0.5
    |> int_of_float in
  let items =
    Array.map (renderResultItem model.visitedRecords) result.records |> Array.to_list
  in
  div [] [
      p [ class' Style.searchResultsInfo ]
        [ text ("Results: " ^ (string_of_int result.resultCount)) ]
    ; ul [ class' Style.searchResults] items
    ; (if model.page < pages then
         match model.nextResult with
         | Loading ->
            div
              [ class' (Style.nextPage ~loading:false) ]
              [ p [] [ text "Loading..."] ]
         | _ ->
            div
              [ class' (Style.nextPage ~loading:true); onClick SearchMore ]
              [ text "Load more" ]
       else
         noNode)
    ]

let results resultList model =
    match resultList with
      | Error e -> statusError e
      | Loading -> statusLoading ()
      | Success res -> renderResults res model
      | _ -> Html.noNode

let hasResults results =
  match results with
  | Success results when results.resultCount > 0 -> true
  | _ -> false
  
let view model =
  div
    [ ]
    [ div []
        [
          div [ class' Style.searchBoxWrapper  ]
            [ form
                [ onCB "submit" "" (fun ev -> ev##preventDefault (); Some(OnSearch)) ]
                [
                  input'
                    [ id "search-field"
                    ; class' Style.searchBox
                    ; type' "search"
                    ; name "lookfor"
                    ; value model.lookfor
                    ; onInput (fun str -> (OnChange str))
                    ] []            
                ; input'
                    [ type' "submit"
                    ; value "Search!"
                    ]
                    []
                ; ( if hasResults model.results = true then
                      a [ onClick OpenFacets ] [ text "facets" ]
                    else
                      noNode
                  )
                ]
            ]
        ]
    ; div []
        [ results model.results model ] 
    ; (Facet.view ~model: model.facetModel ~filters:model.filters |> App.map facetMsg)
    ]

