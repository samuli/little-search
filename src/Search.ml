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
[@@bs.deriving {accessors}]

type model = {
    lookfor: string;
    page: int;
    lastSearch: string option;
    results: Finna.searchResult remoteData list;
    nextResult: Finna.searchResult remoteData;
    visitedRecords: Finna.record array
  }

let init =
  {
    lookfor = "start";
    page = 1;
    lastSearch = None;
    results = [ NotAsked ];
    nextResult = NotAsked;
    visitedRecords = [||]
  }

let getSearchCmd ~lookfor ~page =
  let url = Finna.getSearchUrl ~lookfor ~page in
  Http.send gotResults (Http.getString url)

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
  
let update model = function
  | OnSearch ->
     ( { model with lastSearch = None },
       Router.openUrl (Router.routeToUrl (Search model.lookfor)))
  | Search ->
     let newSearch =
       match model.lastSearch with
       | None -> true
       | Some query -> not (query == model.lookfor) in
     if newSearch then
       let cmd =  getSearchCmd ~lookfor:model.lookfor ~page:1 in
       ( { model with nextResult = Loading }, cmd )
     else
       ( model, Cmd.msg pageLoaded )
  | SearchMore ->
     let page = model.page+1 in
     let cmd =  getSearchCmd ~lookfor:model.lookfor ~page in
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
  | _ -> ( model, Cmd.none )
                     
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
      | NotAsked -> Html.noNode
      | Error e -> statusError e
      | Loading -> statusLoading ()
      | Success res -> resultList res.records model
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
            ]
        ; div []
            (results model.results model) 
        ; a [ onClick SearchMore ] [ text "more" ]
        ]
    ]
