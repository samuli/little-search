open Tea.Html

type statusType =
  | InfoStatus
  | ErrorStatus

let statusLoading (): 'a Vdom.t
  = div [ class' Style.info ] [ text "Loading..." ]

let statusError (e) : 'a Vdom.t
  = div [ class' Style.error ] [ text ("Error: " ^ e) ]

  

      

                                            
