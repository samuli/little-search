open Types

open View
   
open Tea
open Tea.Html


type msg =
  | ShowRecord of string
  | GotResult of (string, string Http.error) Result.t
  | CloseRecord
  | PageLoaded
[@@bs.deriving {accessors}]

type model = {
    record: Finna.recordResult remoteData;
    nextRecord: Finna.recordResult remoteData
  }

let init = {
    record = NotAsked;
    nextRecord = NotAsked
  }

let update model = function
  | ShowRecord id ->
     let url = Finna.getRecordUrl ~id in
     let cmd =  Http.send gotResult (Http.getString url) in
     ( { model with nextRecord = Loading }, cmd )
  | GotResult (Ok data) ->
     let record = Finna.decodeRecordResult data in
     ( { model with record }, Cmd.msg pageLoaded )
  | GotResult (Error e) -> ( { model with record = Error (Http.string_of_error e) }, Cmd.none )
  |_ -> (model, Cmd.none)

let viewRecord (r:Finna.record) =
  div [ ] [
      h1 [] [ text r.title ]
    ; p [] [ text r.id ]
    ; match r.formats with
      | Some formats ->
         let formats = Array.map (fun (f:Finna.translated) -> f.translated) formats in
         let formats = String.concat ", " (Array.to_list formats) in
         p [] [ text formats ]
      | None -> noNode
    ]
  
let view model =
  div
    [ class' Style.recordFull ]
    [
      match model.record with
      | Loading -> statusLoading ()
      | Error e -> statusError e
      | Success res ->
         let r = res.record in
         viewRecord r
      | _ -> Html.noNode
    ]
