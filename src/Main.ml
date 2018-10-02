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
    searchModel: Search.model;
    recordModel: Record.model
}
       
let init () location =
  let route = Router.urlToRoute location in
  let cmd = Cmd.msg (urlChanged location) in
  ({ route;
     searchModel = Search.init;
     recordModel = Record.init }
  , cmd)

let subscriptions _model =
  Sub.none
  
let update model = function
  | SearchMsg subMsg ->
     let (searchModel, cmd) = (Search.update model.searchModel subMsg) in
     ( {model with searchModel}, (Cmd.map searchMsg cmd) )     
  | RecordMsg subMsg ->
     let (recordModel, cmd) = (Record.update model.recordModel subMsg) in
     ( {model with recordModel}, (Cmd.map recordMsg cmd) )     
  | UrlChanged location ->
     let route = Router.urlToRoute location in
     let cmd =
       begin match route with
       | Main -> Cmd.none
       | Search _query -> Cmd.map searchMsg (Cmd.msg Search.search)
       | Record id -> Cmd.map recordMsg (Cmd.msg (Record.showRecord id))
       end in
     ( { model with route }, cmd )
       
let view model =
  div
    [ ]
    [ p
        [ ]
        [ match model.route with
          | Main ->
             div [] [ Search.view model.searchModel |> map searchMsg ]
          | Search _query -> 
              div [] [
                  a [ href (Router.routeToUrl Main)] [(text "main")]
                ; Search.view model.searchModel |> map searchMsg
                ]
          | Record _recordId ->
             Record.view model.recordModel |> map recordMsg
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
    
