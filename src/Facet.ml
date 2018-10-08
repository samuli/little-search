open Types
open Tea
open Tea.Html

type msg =
  | OpenFacets
  | CloseFacets
  | GetFacets of string
  | FacetResults of Finna.filter
[@@bs.deriving {accessors}]

type facet = {
    key: string;
    type': Finna.facetType;
    items: Finna.facetItem array remoteData
  } 
            
type model = {
    isOpen: bool;
    facets: facet Js.Dict.t;
  }

let initFacets =
  let keys = [ "format"; "building" ] in
  let facets = Js.Dict.empty() in
  List.iter (fun key ->
      let facet = {
          key;
          type' = Finna.FacetNormal;
          items = NotAskedType [||]
        } in
      Js.Dict.set facets key facet
    ) keys;
  facets
        
let init = {
    isOpen = false;
    facets = initFacets
  }

let update model = function
  | OpenFacets -> ( { model with isOpen = true }, Cmd.none )
  | CloseFacets -> ( { model with isOpen = false }, Cmd.none )
  | GetFacets _x -> ( model, Cmd.none)
  | FacetResults _x -> ( model, Cmd.none)

let facetList facets =
  let renderFacetItems key items =
    Array.map (fun (item:Finna.facetItem) -> li [ onClick (FacetResults { key; value = item.value}) ] [ text item.label ] ) items
  in
  let facet f =
    let renderFacet key items css =
      li [ class' (Style.facetItem css)
         ; onClick (GetFacets key)
        ]
        [
          p [] [ text key]
        ; ul [] (Array.to_list (renderFacetItems key items))
        ]
    in
    match f.items with
    | Success t -> renderFacet f.key t "loaded"
    | NotAskedType t -> renderFacet f.key t "not-asked"
    | LoadingType t -> renderFacet f.key t "loading"
    | _ -> noNode
  in
  ul [ ]
    (let keys = Js.Dict.keys facets in
     List.map (fun key ->
         match Js.Dict.get facets key with
         | Some f -> facet f
         | None -> noNode
       ) ( Array.to_list keys )
    )
  
let view model =
  if model.isOpen = false then noNode else
    div [ class' Style.facetModal ]
      [
        button [ onClick CloseFacets ] [ text "close" ]
      ; text "facet"
      ; facetList model.facets
      ]
