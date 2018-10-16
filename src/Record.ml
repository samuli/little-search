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
    record: Finna.record remoteData;
    nextRecord: Finna.record remoteData
  }

let init =
  {
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
    ; (match r.formats with
      | Some formats ->
         let formats = Array.map (fun (f:Finna.translated) -> f.translated) formats in
         let formats = String.concat ", " (Array.to_list formats) in
         p [] [ text formats ]
      | None -> noNode)
    ; (match r.images with
       | Some images when (Array.length images) = 0 -> noNode
       | Some images ->
          let item i path imgId =
           let path = Finna.baseUrl ^ path in
           li [] [
               img [
                   id imgId
                 ;  (if i < 4 then src path else href path)
                 ; class' "record-image"
                 ] [] ]
          in
          (* hash record.id to get a working querySelector for inView.js *)
          let hash =
            [%raw {| 
function(s) {
   return s.split("").reduce(function(a,b){a=((a<<5)-a)+b.charCodeAt(0);
return a&a},0);
             }|}]
          in
          let imgId i =
            let id = hash r.id in
            "img-" ^ id ^ "-" ^ (string_of_int i)
          in
          
          let images =
           Array.mapi (fun i path ->
               let id = imgId i in
               item i path id) images
         in
         let ids = Array.mapi (fun i _ -> imgId i) images in
         Js.Global.setTimeout (fun () ->
             View.registerInview (Array.to_list ids)) 100 |> ignore;

         ul [] (Array.to_list images)
      | None -> noNode)
    ]
  
let view model =
  div
    [ class' Style.recordFull ]
    [
      match model.record with
      | Loading -> statusLoading ()
      | Error e -> statusError e
      | Success r -> viewRecord r
      | _ -> Html.noNode
    ]
