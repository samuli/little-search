[%%debugger.chrome]
 
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

let initContext ~language ~limit = {
    language;
    translations = Loading;
    prevRoute = None;
    pagination = { count = 0; items = []; limit};
  }
  
let init () location =
  let language =
    Util.fromStorage "language" (Types.languageCode LngFi)
    |> Types.languageOfCode
  in
  let context = initContext ~language ~limit:0 in
  let route = Router.urlToRoute location in
  let translationsCmd =
    Util.loadTranslations
      (Types.languageCode context.language) gotTranslations
  in
  let urlChangeCmd = Cmd.msg (urlChanged location) in
  ({ route;
     searchModel = Search.init;
     recordModel = Record.init;     
     nextPage = PageReady route;
     context;
   }
  , Cmd.batch [urlChangeCmd; translationsCmd] )
 
let subscriptions _model =
  Sub.none

let pageToRoute page =
  match page with
  | PageLoading route | PageReady route -> route

let handlePageLoaded ~route ~model =
  let _ = match route with
  | SearchRoute _params ->
     (match model.context.prevRoute with
      | Some route ->
         begin 
           match route with
           | RecordRoute id ->
              let id = (Util.hash id) in
              Util.scrollToElement id;
           | _ -> ()
         end
      | _ -> ()
     );
  | RecordRoute _id ->
     Util.resetPageScroll ()
  | _ -> ()
  in
  let route = pageToRoute model.nextPage in
  { model with route; nextPage = PageReady route }
    
let handleOutMsg ~outMsg ~model =
  let context = model.context in
  
  match outMsg with
  | PageLoaded route ->
     let model = handlePageLoaded ~route ~model in
     (model, Cmd.none)
  | UpdateTranslations translations ->
     let context = { context with translations } in
     ( { model with context }, Cmd.none)
  | UpdatePagination pagination -> 
     ( { model with context = { context with pagination } }, Cmd.none)
  | LoadResultsInBackground (page) ->
     let (searchModel, cmd, _) =
       Search.update
         model.searchModel model.context (Search.searchMore (page, true))
     in
     let cmd = Cmd.map searchMsg cmd in
     ( { model with searchModel }, cmd )
  | GotResultsInBackground ->
     let (_recordModel, cmd, _) =
       Record.update
         ~model:model.recordModel
         ~context:model.context
         ~results:model.searchModel.results
         (Record.recordPaginated)
     in
     (model, (Cmd.map recordMsg cmd) )
  | NoUpdate -> (model, Cmd.none)
       
let update model = function
  | ChangeLanguage language ->
     let cmd =
       Util.loadTranslations (Types.languageCode language) gotTranslations in
     let context = { model.context with language } in
     ( { model with context }, cmd)
  | GotTranslations (Ok data) ->
     Util.toStorage "language" (Types.languageCode model.context.language);
     let translations = Util.decodeTranslations data in
     let (model, cmd) =
       handleOutMsg ~outMsg:(UpdateTranslations translations) ~model
     in
     (model, cmd)
  | GotTranslations (Error e) ->
     let translations = Error (Http.string_of_error e) in
     let context = { model.context with translations } in
     ( { model with context }, Cmd.none )

  | SearchMsg subMsg ->
     let (searchModel, cmd, outMsg) =
       Search.update model.searchModel model.context subMsg
     in
     let cmd = Cmd.map searchMsg cmd in
     let model = { model with searchModel } in
     let (model, outCmd) = handleOutMsg ~outMsg ~model in
     ( model, Cmd.batch [cmd; outCmd] )     

  | RecordMsg subMsg ->
     let (recordModel, cmd, outMsg) =
       (Record.update
          ~model:model.recordModel
          ~context:model.context
          ~results:model.searchModel.results subMsg)
     in
     let cmd = Cmd.map recordMsg cmd in
     let model = { model with recordModel } in
     let (model, outCmd) = handleOutMsg ~outMsg ~model in
     ( model, Cmd.batch [cmd; outCmd] )

  | UrlChanged location ->
     let route = Router.urlToRoute location in
     let (nextPage, cmd) =
       begin match (route:Types.route) with
       | MainRoute -> ((PageReady MainRoute), Cmd.none)
       | SearchRoute query ->
          ((PageLoading (SearchRoute query)),
           Cmd.map searchMsg (Cmd.msg (Search.search query)))
       | RecordRoute id ->
          ((PageLoading (RecordRoute id)),
           Cmd.map recordMsg (Cmd.msg (Record.showRecord id)))
       end in
     let context = { model.context with prevRoute = Some model.route } in
     ( { model with context; nextPage }, cmd )

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
    | PageLoading _route -> true
    | PageReady _route -> false
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
          ; p
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
                 Record.view
                   ~model:model.recordModel
                   ~context:model.context
                   ~results:model.searchModel.results
                   ~limit:model.searchModel.searchParams.limit
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
    
