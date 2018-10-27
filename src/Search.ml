open Types
open Finna

open View
   
open Tea
open Tea.Html

type searchDir = PrevPage | NextPage
                      
type msg =
  | OnSearch
  | Search of Types.searchParams
  | SearchMore of searchDir
  | OnChange of string
  | GotResults of (string, string Http.error) Result.t
  | PageLoaded
  | RemoveFilter of (string)
  | OpenFacets
  | FacetMsg of Facet.msg
  | GotFacets of (string, string Http.error) Result.t
[@@bs.deriving {accessors}]


type searchResultPageType = {
    page: int;
    results: Finna.searchResult;
  }
type searchResultsType = {
    count: int;
    pageCount: int;
    pages: searchResultPageType remoteData array;
  }
type model = {
    searchParams: Types.searchParams;
    results: searchResultsType;
    lastSearch: string option;
    nextResult: Finna.searchResult remoteData;
    visitedRecords: Finna.record array;
    facetsOpen: bool;
    facetModel: Facet.model
  }


let initResults =
  {
    count = 0;
    pageCount = 0;
    pages = [||];
  }

let init =
  {
    searchParams = {
      lookfor = "";
      page = 0;
      limit = 3;
      filters = [];
    };
    lastSearch = None;
    results = initResults;
    nextResult = NotAsked;
    visitedRecords = [||];
    facetsOpen = false;
    facetModel = Facet.init
  }

let getHttpCmd callback url =
  Http.send callback (Http.getString url)
               
let getSearchCmd ~(params:Types.searchParams) ~lng =
  let lng = Types.finnaLanguageCode lng in
  let params = { params with page = params.page+1 } in
  let url = Finna.getSearchUrl ~params ~lng in
  getHttpCmd gotResults url
  
let appendResults ~model ~newResults =
  let page = model.searchParams.page in

  let (resultPage, count)  = match newResults with
    | Success res -> begin
        let count = res.resultCount in
        let resultPage = (Success { page; results = res }) in
        (resultPage, count)
      end
    | _ -> ((Error ""), 0)
  in

  let pages = Array.append model.results.pages [| resultPage |] in
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
  let searchParams = { model.searchParams with filters = Array.to_list filters } in
  let cmd = Router.openUrl (Router.routeToUrl (SearchRoute searchParams)) in
  let model = { model with searchParams;
                           lastSearch = None;
                           nextResult = Loading }
  in
  (cmd, model)
  
let update model context = function
  | OnSearch ->
     ( { model with lastSearch = None },
       Router.openUrl (Router.routeToUrl (SearchRoute model.searchParams)),
       NoUpdate
     )
  | Search params ->
     let newSearch =
       match model.lastSearch with
       | None -> true
       | Some query -> not (query == params.lookfor) in
     let model = {
         model with nextResult = Loading;
                    searchParams = params }
     in
     let model = if newSearch then
                   { model with results = initResults }
                 else
                   model
     in
     let cmd = getSearchCmd ~params ~lng:context.language in
     ( model, cmd, NoUpdate )
  | SearchMore dir ->
     let page = model.searchParams.page in
     let page = match dir with
       | NextPage -> page+1
       | PrevPage -> page-1
     in
     let searchParams =
       { model.searchParams with page } in
     ( { model with searchParams; nextResult = Loading },
       Router.openUrl (Router.routeToUrl (SearchRoute searchParams)),
       NoUpdate )

     (* let cmd =
      *   getSearchCmd ~params:searchParams ~lng:context.language in
      * ( { model with
      *     nextResult = Loading;
      *     lastSearch = Some model.searchParams.lookfor;
      *     searchParams;
      *   }, cmd, NoUpdate ) *)

  | OnChange lookfor ->
     let searchParams = { model.searchParams with lookfor } in
     ( { model with searchParams } , (Cmd.none), NoUpdate )
  | GotResults (Ok data) ->
     let result = Finna.decodeSearchResults data in
     let model = appendResults ~model ~newResults: result in
     let recIds = [] in
     
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
          ~filters:model.searchParams.filters subMsg) (* TODO *)
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
        ; (match r.title with
           | Some title when title <> "" -> h1 [] [ text title ]
           | _ -> noNode)
        ; Record.formats r.formats
        ; Record.publishInfo r
        ; Record.buildings r.buildings
        ]
    ]

let isPageLoading ~page ~results =
  Js.log (Printf.sprintf "isloading %d, len%d" page (Array.length results));
  if Array.length results > (page-1) then false else
    begin
      Js.log "2";
      match results.(page) with
      | Loading -> true
      | _ -> false
    end
  
let resultPageLoadNeighbor ~dir ~page ~results ~context =
  Js.log (Printf.sprintf "neigh: %d" page);
  if isPageLoading ~page ~results then
     div
       [ class' (Style.nextPage ~loading:false) ]
       [ p [] [ text (Util.trans "Loading..." context.translations)] ]
  else
    div
       [ class' (Style.nextPage ~loading:true); onClick (SearchMore dir) ]
       [ text (Util.trans "Search more" context.translations) ]
  
let renderResultPage (result:searchResultPageType) (model:model) context =
  let page = result.page in
  let records = result.results.records in
  let items =
    Array.map
      (renderResultItem model.visitedRecords)
      records
    |> Array.to_list
  in
  div [] [
      (if page > 0 then
         resultPageLoadNeighbor ~dir:PrevPage ~page:(page-1) ~results:model.results.pages ~context
       else
         noNode)
    ; ul [ class' Style.searchResults] items
    ; (if page < (model.results.pageCount-1) then
       resultPageLoadNeighbor ~dir:NextPage ~page ~results:model.results.pages ~context
       else
         noNode)
    ]

let renderResultPage ~page ~model ~context =
    match (page) with
      | Error e -> statusError e
      | Loading -> statusLoading ()
      | Success res -> renderResultPage res model context
      | _ -> Html.noNode

let results ~results ~model ~context =
  div []
    [
      p [ class' Style.searchResultsInfo ]
        [ text (Printf.sprintf "%s: %d"
                  (Util.trans "Results" context.translations)
                  results.count)
        ]
    ; div [] 
        ((Array.map (fun page->
              renderResultPage ~page ~model ~context) results.pages)
         |> Array.to_list)
    ]
  
let hasResults results =
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
    ; div []
        [ results ~results:model.results ~model ~context ] 
    ; (Facet.view
         ~model: model.facetModel
         ~context: context
         ~filters:model.searchParams.filters |> App.map facetMsg)
    ]

