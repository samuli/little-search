open Types
open Finna

open View
   
open Tea
open Tea.Html

type msg =
  | OnSearch
  | Search
  | OnChange of string
  | GotResults of (string, string Http.error) Result.t
  | ShowRecord of Finna.record
[@@bs.deriving {accessors}]

type model = {
    lookfor: string;
    result: Finna.searchResult remoteData;
    visitedRecords: Finna.record array
  }

let init = {
    lookfor = "start";
    result = NotAsked;
    visitedRecords = [||]
  }

let update model = function
  | OnSearch -> ( model, Router.openUrl (Router.routeToUrl (Search model.lookfor)))
  | Search ->
     let url = Finna.getSearchUrl ~lookfor:model.lookfor in
     let cmd =  Http.send gotResults (Http.getString url) in
     ( { model with result = Loading }, cmd )
  | OnChange lookfor -> ( { model with lookfor } , (Cmd.none) )
  | GotResults (Ok data) ->
     let result = Finna.decodeSearchResults data in
     ( { model with result }, Cmd.none )
  | GotResults (Error e) ->
     let result = Error (Http.string_of_error e) in
     ( { model with result }, Cmd.none )
  | ShowRecord r ->
     let cmd = Router.openRoute (Record r.id) in
     let visitedRecords = Array.append model.visitedRecords [| r |] in
     ( { model with visitedRecords }, cmd )
                     
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
  ul [ class' Style.searchResults] items

let view model =
  div
    [ ]
    [ div [ class' Style.searchBoxWrapper ]
        [ input'
            [ class' Style.searchBox
            ; type' "text"
            ; name "lookfor"
            ; value model.lookfor
            ; onInput (fun str -> (OnChange str))
            ] []
        ]
     ; input' [ type' "submit"
              ; onClick onSearch
              ; value "Search!"
         ] []
    ; match model.result with
      | NotAsked -> Html.noNode
      | Loading -> statusLoading ()
      | Error e -> statusError e
      | Success res -> resultList res.records model
    ]
