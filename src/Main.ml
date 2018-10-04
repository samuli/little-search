open Tea
open App
open Tea.Html
open Types


let _ = Style.init


type msg =
  | UrlChanged of Web.Location.location
  | SearchMsg of Search.msg
  | RecordMsg of Record.msg
[@@bs.deriving {accessors}]

type model = {
    route: route;
    page: page;
    pageLoading: bool;
    searchModel: Search.model;
    recordModel: Record.model;
}
       
let init () location =
  let route = Router.urlToRoute location in
  let cmd = Cmd.msg (urlChanged location) in
  ({ route;
     searchModel = Search.init;
     recordModel = Record.init;
     page = route;
     pageLoading = false
   }
  , cmd)

let subscriptions _model =
  Sub.none

let update model = function
  | SearchMsg subMsg ->
     begin match subMsg with
     | Search.PageLoaded -> 
        ( { model with page = model.route; pageLoading = false }, Cmd.none )
     | _ ->
       let (searchModel, cmd) = (Search.update model.searchModel subMsg) in
       ( {model with searchModel}, (Cmd.map searchMsg cmd) )
     end
  | RecordMsg subMsg ->
     begin match subMsg with
     | Record.PageLoaded -> 
        ( { model with page = model.route; pageLoading = false }, Cmd.none )
     | _ ->
       let (recordModel, cmd) = (Record.update model.recordModel subMsg) in
       ( {model with recordModel}, (Cmd.map recordMsg cmd) )
     end
  | UrlChanged location ->
     let route = Router.urlToRoute location in
     let cmd =
       begin match route with
       | Main -> Cmd.none
       | Search _query -> Cmd.map searchMsg (Cmd.msg Search.search)
       | Record id -> Cmd.map recordMsg (Cmd.msg (Record.showRecord id))
       end in
     ( { model with route; pageLoading = true }, cmd )
       
let view model =
  div []
    [
      div [
          class' (Style.loadingIndicator ~show: model.pageLoading)
        ] [ text "Loading..." ]
    ; div
        [ ]
        [ p
            [ ]
            [ match model.page with
              | Main ->
                 div [] [ Search.view model.searchModel |> map searchMsg ]
              | Search _query -> 
                 div [ ] [
                     Search.view model.searchModel |> map searchMsg
                   ]
              | Record _recordId ->
                 Record.view model.recordModel |> map recordMsg
            ]
        ]
    ]

let main =
  Navigation.navigationProgram urlChanged {
    init;
    update;
    view;
    subscriptions;
    shutdown = (fun _ -> Cmd.none)
    }
    
