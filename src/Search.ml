open Types
open Finna

open View
   
open Tea
open Tea.Html

type msg =
  | OnSearch
  | Search of (string * Types.searchParams)
  | SearchMore
  | OnChange of string
  | GotResults of (string, string Http.error) Result.t
  | PageLoaded
  | RemoveFilter of (string)
  | OpenFacets
  | FacetMsg of Facet.msg
  | GotFacets of (string, string Http.error) Result.t
[@@bs.deriving {accessors}]

                  
type model = {
    searchParams: Types.searchParamsType;
    results: Finna.searchResult remoteData;
    lastSearch: string option;
    nextResult: Finna.searchResult remoteData;
    visitedRecords: Finna.record array;
    facetsOpen: bool;
    facetModel: Facet.model
  }

let init =
  {
    searchParams = {
      lookfor = "";
      page = 1;
      limit = 30;
      filters = [||];
    };
    lastSearch = None;
    results = NotAsked;
    nextResult = NotAsked;
    visitedRecords = [||];
    facetsOpen = false;
    facetModel = Facet.init
  }

let getHttpCmd callback url =
  Http.send callback (Http.getString url)
               
let getSearchCmd ~params ~lng =
  let lng = Types.finnaLanguageCode lng in
  let url = Finna.getSearchUrl ~params ~lng in
  getHttpCmd gotResults url
  
let appendResults ~model ~newResults =
  let allResults = match (model.results, newResults, model.lastSearch) with
    | (NotAsked, _, _) | (_, _, None) -> newResults
    | (Success result, Success newRes, _) ->
       let records = (List.append
                        (Array.to_list result.records)
                        (Array.to_list newRes.records)) |> Array.of_list
       in
       Success { result with records }
    | (t, _, _) -> t 
  in
  { model with
    results = allResults;
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
  let filters = Array.to_list model.searchParams.filters in
  let filters = 
    if mode then
      let filters = Array.to_list model.searchParams.filters in
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
  let cmd =
    Router.openUrl
      (Router.routeToUrl
         (SearchRoute
            (model.searchParams.lookfor, (Array.to_list filters)))) in
  let searchParams =
    { model.searchParams with filters } in
  let model = { model with searchParams;
                           lastSearch = None;
                           nextResult = Loading }
  in
  (cmd, model)
  
let update model context = function
  | OnSearch ->
     let params = (model.searchParams.lookfor,
                   Array.to_list model.searchParams.filters)
     in
     ( { model with lastSearch = None },
       Router.openUrl (Router.routeToUrl (SearchRoute params)),
       NoUpdate
     )
  | Search (lookfor, filters) ->
     let newSearch =
       match model.lastSearch with
       | None -> true
       | Some query -> not (query == model.searchParams.lookfor) in
     if newSearch then
       let filters = Array.of_list filters in
       let params = { model.searchParams with lookfor; filters; page = 1 } in
       let cmd =
         getSearchCmd ~params ~lng:context.language in
       ( { model with searchParams = params; nextResult = Loading },
         cmd, NoUpdate )
     else
       ( model, Cmd.msg pageLoaded, NoUpdate )
  | SearchMore ->
     let searchParams =
       { model.searchParams with page = model.searchParams.page+1 } in
     let cmd =
       getSearchCmd ~params:searchParams ~lng:context.language in
     ( { model with
         nextResult = Loading;
         lastSearch = Some model.searchParams.lookfor;
         searchParams;
       }, cmd, NoUpdate )
  | OnChange lookfor ->
     let searchParams = { model.searchParams with lookfor } in
     ( { model with searchParams } , (Cmd.none), NoUpdate )
  | GotResults (Ok data) ->
     let result = Finna.decodeSearchResults data in
     let model = appendResults ~model ~newResults: result in
     let recIds = match model.results with
       | Success res -> Array.map (fun r -> r.id) res.records |> Array.to_list
       | _ -> []
     in
     ( model, Cmd.msg pageLoaded, UpdateRecordIds recIds )
  | GotResults (Error e) ->
     let result = Error (Http.string_of_error e) in
     let model = appendResults ~model ~newResults: result in
     ( model, Cmd.msg pageLoaded, NoUpdate )
  | PageLoaded -> ( model, Cmd.none, NoUpdate )
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
              (Array.to_list model.searchParams.filters)
          in
          let filters = Array.of_list filters in
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
              (fun (f:Finna.facetItem) -> (f.value, f.translated))
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

     
let renderResultItem visitedRecords r =
  let visited =
    begin try
        let _el =
          List.find (fun el -> el.id = r.id)
            (Array.to_list visitedRecords)
        in
        true
      with Not_found -> false
    end in
  a [ href (Router.routeToUrl (RecordRoute r.id))        
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

let renderResults (result:Finna.searchResult) (model:model) context =
  let page = model.searchParams.page in
  let limit = model.searchParams.limit in
  let pages =
    floor ((float_of_int result.resultCount) /. (float_of_int limit)) +. 0.5
    |> int_of_float in
  let items =
    Array.map
      (renderResultItem model.visitedRecords)
      result.records
    |> Array.to_list
  in
  div [] [
      p [ class' Style.searchResultsInfo ]
        [ text (Printf.sprintf "%s: %d"
                  (Util.trans "Results" context.translations)
                  result.resultCount)
        ]
    ; ul [ class' Style.searchResults] items
    ; (if page < pages then
         match model.nextResult with
         | Loading ->
            div
              [ class' (Style.nextPage ~loading:false) ]
              [ p [] [ text (Util.trans "Loading..." context.translations)] ]
         | _ ->
            div
              [ class' (Style.nextPage ~loading:true); onClick SearchMore ]
              [ text (Util.trans "Search more" context.translations) ]
       else
         noNode)
    ]

let results resultList model context =
    match resultList with
      | Error e -> statusError e
      | Loading -> statusLoading ()
      | Success res -> renderResults res model context
      | _ -> Html.noNode

let hasResults results =
  match results with
  | Success results when results.resultCount > 0 -> true
  | _ -> false

let filters filters context =
  let items =
    Array.map (fun (key, value) ->
        let value = Util.trans value context.translations in 
        p [ onClick (RemoveFilter key) ]
          [ text value ] ) filters
  in
  div [] (Array.to_list items)
  
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
    ; div []
        [ results model.results model context ] 
    ; (Facet.view
         ~model: model.facetModel
         ~context: context
         ~filters:model.searchParams.filters |> App.map facetMsg)
    ]

