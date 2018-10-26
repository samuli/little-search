open Tea
open App
open Tea.Html
open Types

let _ = Style.init

type msg =
  | ChangeLanguage of language
  | GotTranslations of (string, string Http.error) Result.t
  | UrlChanged of Web.Location.location
  | SearchMsg of Search.msg
  | RecordMsg of Record.msg
[@@bs.deriving {accessors}]

type model = {
    route: route;
    nextPage: page;
    searchModel: Search.model;
    recordModel: Record.model;
    context: Types.context;
}

let initContext ~language = {
    language;
    translations = Loading;
    recordIds = []
  }
  
let init () location =
  let language =
    Util.fromStorage "language" (Types.languageCode LngFi)
    |> Types.languageOfCode
  in
  let context = initContext ~language in
  let route = Router.urlToRoute location in
  let translationsCmd =
    Util.loadTranslations (Types.languageCode context.language) gotTranslations in
  let urlChangeCmd = Cmd.msg (urlChanged location) in
  ({ route;
     searchModel = Search.init;
     recordModel = Record.init;     
     nextPage = Ready route;
     context;
   }
  , Cmd.batch [urlChangeCmd; translationsCmd] )

let subscriptions _model =
  Sub.none

let pageToRoute page =
  match page with
  | Loading route | Ready route -> route

let updateContext cmd context =
  match cmd with
  | UpdateTranslations translations -> { context with translations }
  | UpdateRecordIds recordIds -> { context with recordIds }
  | NoUpdate -> context
       
let update model = function
  | ChangeLanguage language ->
     let cmd =
       Util.loadTranslations (Types.languageCode language) gotTranslations in
     let context = { model.context with language } in
     ( { model with context }, cmd)
  | GotTranslations (Ok data) ->
     Util.toStorage "language" (Types.languageCode model.context.language);
     let translations = Util.decodeTranslations data in
     let context =
       updateContext (UpdateTranslations translations) model.context
     in
     ( { model with context }, Cmd.none )
  | GotTranslations (Error e) ->
     let translations = Error (Http.string_of_error e) in
     let context = { model.context with translations } in
     ( { model with context }, Cmd.none )
  | SearchMsg subMsg ->
     begin match subMsg with
     | Search.PageLoaded ->
        let route = pageToRoute model.nextPage in
        ( { model with route; nextPage = Ready route }, Cmd.none )
     | _ ->
        let (searchModel, cmd, contextCmd) =
          Search.update model.searchModel model.context subMsg
        in
        let context = updateContext contextCmd model.context in
       ( {model with searchModel; context}, (Cmd.map searchMsg cmd) )
     end
  | RecordMsg subMsg ->
     begin match subMsg with
     | Record.PageLoaded ->
        let _ = Util.resetPageScroll () in
        let route = pageToRoute model.nextPage in
        ( { model with route; nextPage = Ready route }, Cmd.none )
     | _ ->
        let (recordModel, cmd) =
          (Record.update model.recordModel model.context subMsg)
        in
       ( {model with recordModel}, (Cmd.map recordMsg cmd) )
     end
  | UrlChanged location ->
     let route = Router.urlToRoute location in
     let (nextPage, cmd) =
       begin match (route:Types.route) with
       | MainRoute -> ((Ready MainRoute), Cmd.none)
       | SearchRoute query ->
          ((Loading (SearchRoute query)),
           Cmd.map searchMsg (Cmd.msg (Search.search query)))
       | RecordRoute id ->
          ((Loading (RecordRoute id)),
           Cmd.map recordMsg (Cmd.msg (Record.showRecord id)))
       end in
     ( { model with nextPage }, cmd )

let languageMenu context =
  let item ~lng ~currentLng =
    let active = lng = currentLng in
    li [
        class' (Style.language ~active)
      ; onClick (changeLanguage lng)
      ]
      [ div [] [ text (Types.languageCode lng) ] ]
  in
    
  let currentLng = context.language in
  div [] [
    ul [ class' Style.languageMenu ] [
          item ~lng:LngFi ~currentLng 
        ; item ~lng:LngEn ~currentLng 
        ]
    ]
  
let view model =
  let pageLoading = match model.nextPage with
    | Loading _route -> true
    | Ready _route -> false
  in
  div []
    [
      div [
          class' (Style.loadingIndicator ~show: pageLoading)
        ] [ text (Util.trans "Loading..." model.context.translations) ]
    ; div
        [ ]
        [
          (languageMenu model.context)
          ;p
            [ ]
            [ match model.route with
              | MainRoute ->
                 div [] [ Search.view model.searchModel model.context
                          |> map searchMsg ]
              | SearchRoute _query -> 
                 div [] [
                     Search.view model.searchModel model.context
                     |> map searchMsg
                   ]
              | RecordRoute _recordId ->
                 Record.view model.recordModel model.context
                 |> map recordMsg
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
    
