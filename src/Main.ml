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
    nextPage: page;
    searchModel: Search.model;
    recordModel: Record.model;
}
       
let init () location =
  let route = Router.urlToRoute location in
  let cmd = Cmd.msg (urlChanged location) in
  ({ route;
     searchModel = Search.init;
     recordModel = Record.init;     
     nextPage = Ready route;
   }
  , cmd)

let subscriptions _model =
  Sub.none

let pageToRoute page =
  match page with
  | Loading route | Ready route -> route
             
let update model = function
  | SearchMsg subMsg ->
     begin match subMsg with
     | Search.PageLoaded ->
        let route = pageToRoute model.nextPage in
        ( { model with route; nextPage = Ready route }, Cmd.none )
     | _ ->
        let (searchModel, cmd) =
          Search.update model.searchModel subMsg
        in
       ( {model with searchModel}, (Cmd.map searchMsg cmd) )
     end
  | RecordMsg subMsg ->
     begin match subMsg with
     | Record.PageLoaded ->
        let route = pageToRoute model.nextPage in
        ( { model with route; nextPage = Ready route }, Cmd.none )
     | _ ->
        let (recordModel, cmd) =
          (Record.update model.recordModel subMsg)
        in
       ( {model with recordModel}, (Cmd.map recordMsg cmd) )
     end
  | UrlChanged location ->
     let route = Router.urlToRoute location in
     let (nextPage, cmd) =
       begin match route with
       | Main -> ((Ready Main), Cmd.none)
       | Search query ->
          ((Loading (Search query)),
           Cmd.map searchMsg (Cmd.msg Search.search))
       | Record id ->
          ((Loading (Record id)),
           Cmd.map recordMsg (Cmd.msg (Record.showRecord id)))
       end in
     ( { model with nextPage }, cmd )
       
let view model =
  let pageLoading = match model.nextPage with
    | Loading _route -> true
    | Ready _route -> false
  in
  div []
    [
      div [
          class' (Style.loadingIndicator ~show: pageLoading)
        ] [ text "Loading..." ]
    ; div
        [ ]
        [ p
            [ ]
            [ match model.route with
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
    
