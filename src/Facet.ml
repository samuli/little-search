open Types
open Tea
open Tea.Html

type msg =
  | OpenFacets
  | CloseFacets
  | GetFacets of string
  | ToggleFacet of (bool * Finna.filter)
[@@bs.deriving {accessors}]

type facet = {
    key: string;
    type': Finna.facetType;
    items: Finna.facetItem array remoteData;
    lookfor: string;
  } 
            
type model = {
    isOpen: bool;
    lookfor: string;
    facets: facet Js.Dict.t;
    filters: Finna.filter array
  }

let initFacets lookfor =
  let keys = [ "format"; "building" ] in
  let facets = Js.Dict.empty() in
  List.iter (fun key ->
      let facet = {
          key;
          type' = Finna.FacetNormal;
          items = NotAskedType [||];
          lookfor
        } in
      Js.Dict.set facets key facet
    ) keys;
  facets

let init = {
    isOpen = false;
    lookfor = "";
    facets = initFacets "";
    filters = [||];
  }

let update ~model ~lookfor ~filters = function
  | OpenFacets -> (
    if lookfor = model.lookfor && filters = model.filters then
      ( { model with isOpen = true }, Cmd.none)
    else
      let entries = Js.Dict.entries model.facets |> Array.to_list in
      let opened =
        List.filter (fun (_key, facet) ->
            match facet.items with
            | NotAskedType _f -> false
            | _ -> true)
          entries
      in
      let cmds = List.map (fun (key, _facet) -> Cmd.msg (getFacets key)) opened in
      { model with lookfor; filters; isOpen = true }, Cmd.batch cmds )
  | CloseFacets -> ( { model with isOpen = false }, Cmd.none )
  | GetFacets _x -> ( model, Cmd.none)
  | ToggleFacet (_, _ ) ->
     ( { model with isOpen = false }, Cmd.none)

let isFacetActive ~filters ~facetKey ~facetValue =
  List.exists
    (fun (f:Finna.filter) -> (facetKey = f.key && facetValue = f.value))
    (Array.to_list filters)
  
let facetList ~facets ~filters =
  let renderFacetItem ~key ~(item:Finna.facetItem) ~filters =
    let isActive =
      isFacetActive ~filters ~facetKey:key ~facetValue:item.value
    in
    li [
        onClick (ToggleFacet ((not isActive), { key; value = item.value}))
      ; class' (Style.facetItem isActive)
      ]
      [ text (item.translated ^ (Printf.sprintf " (%d)" item.count)) ]
  in  
  let renderFacetItems ~key ~items ~filters =
    Array.map (fun item -> renderFacetItem ~key ~item ~filters) items
  in
  let facet f =
    let renderFacet ~key ~items ~css ~filters =
      li [ class' (Style.facet css)
        ]
        [
          p [ onClick (GetFacets key) ] [ text key]
        ; ul [] (Array.to_list (renderFacetItems ~key ~items ~filters))
        ]
    in
    match f.items with
    | Success t -> renderFacet ~key:f.key ~items:t ~css:"loaded" ~filters
    | NotAskedType t -> renderFacet ~key:f.key ~items:t ~css:"not-asked" ~filters
    | LoadingType t -> renderFacet ~key:f.key ~items:t ~css:"loading" ~filters
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
  
let view model filters =
  if model.isOpen = false then noNode else
    div [ class' Style.facetModal ]
      [
        button [ onClick CloseFacets ] [ text "close" ]
      ; text "facet"
      ; facetList ~facets:model.facets ~filters
      ]
