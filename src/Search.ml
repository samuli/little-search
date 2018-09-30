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
[@@bs.deriving {accessors}]

type model = {
    lookfor: string;
    result: Finna.searchResult remoteData;
    cnt: int
  }

let init = { lookfor = "start"; result = NotAsked; cnt = 0 }

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
  | GotResults (Error e) -> ( { model with result = Error (Http.string_of_error e) }, Cmd.none )

let recordItem r =
  let url = Router.routeToUrl (Record r.id) in
  div [] [ a [ href url ] [ text r.title ] ]

let resultList records =
  let items = Array.map recordItem records |> Array.to_list in
  div [] items

let view model =
  div
    []
    [ p [] [ text model.lookfor ]
     ;input' [ type' "text"
             ; name "lookfor"
             ; value model.lookfor
             ; onInput (fun str -> (OnChange str))
        ] []
     ; input' [ type' "submit"
              ; onClick onSearch
              ; value "Search!"
         ] []
    ; match model.result with
      | NotAsked -> status "not asked"
      | Loading -> status "loading"
      | Error e -> status ("error: " ^ e)
      | Success res -> resultList res.records
    ]
