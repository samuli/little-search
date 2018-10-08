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
    lookfor: string;
    page: int;
    limit: int;
    filters: Finna.filter array;
    lastSearch: string option;
    results: Finna.searchResult remoteData list;
    nextResult: Finna.searchResult remoteData;
    visitedRecords: Finna.record array;
    facetsOpen: bool;
    facetModel: Facet.model
  }

let init =
  {
    lookfor = "start";
    page = 1;
    limit = 30;
    filters = [||];
    lastSearch = None;
    results = [ NotAsked ];
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
  let allResults = match model.lastSearch with
    | None -> [ newResults ]
    | Some _query -> List.append model.results [ newResults ]
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
       | NotAskedType t | LoadingType t | Success t ->
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
     Js.log(result);
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
     begin
       match subMsg with
       | Facet.GetFacets facet ->
          let url = Finna.getFacetSearchUrl ~lookfor:model.lookfor ~page:model.page ~facet in
          let cmd = getHttpCmd gotFacets url in
          let facets = updateFacet ~facets:model.facetModel.facets ~key:facet ~mode:"loading" ~items:[||] in
          ( { model with facetModel = { model.facetModel with facets} }, cmd )
       | Facet.FacetResults filter ->
          let filters = Array.append model.filters [| filter |] in
          ( { model with filters; lastSearch = None }, Cmd.msg search )
       | _ ->
          let (facetModel, cmd) = (Facet.update model.facetModel subMsg) in
          ( {model with facetModel}, (Cmd.map facetMsg cmd) )
     end
  | GotFacets (Ok data) ->
     let facets = match Finna.decodeFacetResults data with
       | Success (key, items) ->
          updateFacet ~facets:model.facetModel.facets ~key ~mode:"success" ~items
       | _ -> model.facetModel.facets in
     ( { model with facetModel = { model.facetModel with facets} }, Cmd.none )
  | GotFacets (Error _e) ->
     ( model, Cmd.none )

     
let recordItem visitedRecords r =
  let visited =
    begin try
        let _el = List.find (fun el -> el.id = r.id) (Array.to_list visitedRecords) in
        true
      with Not_found -> false
    end in
  a [ onClick (ShowRecord r)
    ]
    [ li [ class' (Style.recordListBkg ~visited) ]
        [ text r.title ]
    ]

let resultList records model =
  let items = Array.map (recordItem model.visitedRecords) records |> Array.to_list in
  div [] [
  ul [ class' Style.searchResults] items
    ]

let results resultLists model =
  List.map (fun result ->
    match result with
      | Error e -> statusError e
      | Loading -> statusLoading ()
      | Success res -> resultList res.records model
      | _ -> Html.noNode
    ) resultLists

let view model =
  div
    [ ]
    [ div [ class' Style.searchBoxWrapper  ]
        [ form
            [ onCB "submit" "" (fun ev -> ev##preventDefault (); Some(OnSearch)) ]
            [
              input'
                [ id "search-field"
                ; class' Style.searchBox
                ; type' "text"
                ; name "lookfor"
                ; value model.lookfor
                ; onInput (fun str -> (OnChange str))
                ] []            
            ; input'
                [ type' "submit"
                ; onClick onSearch
                ; value "Search!"
                ]
                []
            ; a [ onClick OpenFacets ] [ text "facets" ]
            ]
        ; div []
            (results model.results model) 
        ; a [ onClick SearchMore ] [ text "more" ]
        ; (Facet.view model.facetModel |> App.map facetMsg)
        ]
    ]
