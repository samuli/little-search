open Tea.Html

type statusType =
  | InfoStatus
  | ErrorStatus

let statusLoading (): 'a Vdom.t
  = div [ class' Style.info ] [ text "Loading..." ]

let statusError (e) : 'a Vdom.t
  = div [ class' Style.error ] [ text ("Error: " ^ e) ]

type iw
type el = Webapi.Dom.Element.t
external inview: string -> iw = "default" [@@bs.module "../node_modules/in-view/src/in-view"]
external on: iw -> string -> (el -> unit) -> unit = "" [@@bs.send]

let registerInview = fun ids ->
  let ids = List.map (fun id -> "#" ^ id) ids in
  let ids = String.concat "," ids in
  let i = inview ids in
  on i "enter" (fun el ->
      match Webapi.Dom.Element.getAttribute "data-inview" el with
      | Some _val -> ()
      | _ ->
         begin
           Webapi.Dom.Element.setAttribute "data-inview" "1" el;
           match Webapi.Dom.Element.getAttribute "href" el with
           | Some(url) -> Webapi.Dom.Element.setAttribute "src" url el
           | _ -> ()
         end
    )
 


      

                                            
