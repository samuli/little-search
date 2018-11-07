open Types
open Tea
open Tea.Html

type msg =
  | OpenFacets
  | CloseFacets
  | GetFacets of string
  | ToggleFacet of string
  | ToggleFacetItem of (bool * searchParam)
[@@bs.deriving {accessors}]

type facet = {
    key: string;
    type': Types.facetType;
    items: Types.facetItem array remoteData;
    lookfor: string;
    opened: bool;
  } 

type model = {
    isOpen: bool;
    lookfor: string;
    facets: facet Js.Dict.t;
    filters: Types.searchParam list
  }

let initFacets lookfor =
  let keys = [
      (Types.FacetBoolean, "online_boolean");
      (Types.FacetNormal, "format");
      (Types.FacetNormal, "building");
      (Types.FacetNormal, "language");
      (Types.FacetNormal, "topic_facet")
    ] in
  let facets = Js.Dict.empty() in
  List.iter (fun (type', key) ->
      let facet = {
          key;
          type';
          items = NotAskedType [||];
          lookfor;
          opened = false
        } in
      Js.Dict.set facets key facet
    ) keys;
  facets

let init = {
    isOpen = false;
    lookfor = "";
    facets = initFacets "";
    filters = [];
  }

let toggleFacetMenu ~mode ~model =
  Style.pageYScroll ~allow:(not mode);
  { model with isOpen = mode }
    
let update ~model ~lookfor ~filters = function
  | OpenFacets ->
    let model = toggleFacetMenu ~mode:true ~model in 
    if lookfor = model.lookfor && filters = model.filters then
      begin
        ( model, Cmd.none)
      end
    else
      let entries = Js.Dict.entries model.facets |> Array.to_list in

      (* toggle previously opened facets open *)
      let opened =
        List.filter (fun (_key, facet) ->
            match (facet, facet.items) with
            | (_facet, NotAskedType _f) -> false
            | (facet, _) when facet.opened = false -> false
            | (_, _) -> true)
          entries
      in
      
      let cmds =
        List.map (fun (key, _facet) -> Cmd.msg (getFacets key)) opened
      in

      let closed =
        List.filter (fun f1 ->
            not (List.exists (fun f2 -> f1 = f2) opened))
          entries
      in

      (* toggle active but closed facets open *)
      (* let activeButClosed =
       *   List.filter (fun (key, _value) ->
       *       not (List.exists
       *         (fun (facetKey, _facetValue) -> facetKey = key) opened))
       *     filters
       * in           *)
      (* let cmds2 =
       *   List.map (fun (key, _facet) ->
       *       Cmd.msg (toggleFacet key)) activeButClosed
       * in *)
      let facets = model.facets in
      List.iter (fun (key, _a) ->
          match (Js.Dict.get facets key) with
          | Some (facet) ->
             (Js.Dict.set facets key { facet with items = NotAskedType [||] });
          | _ -> ()) closed;

      ( { model with lookfor; filters; facets },
        Cmd.batch cmds )  
  | CloseFacets ->
    let model = toggleFacetMenu ~mode:false ~model in
    ( model, Cmd.none )
  | ToggleFacet key ->
     let facets = model.facets in
     let (facets, cmd) = match Js.Dict.get facets key with
       | Some (facet) ->
          let opened = not facet.opened in
          let facet = { facet with opened } in
          let cmd = if opened && facet.items = (NotAskedType [||]) then Cmd.msg (getFacets key) else Cmd.none in
          Js.Dict.set facets key facet;
          (facets, cmd)
       | _ -> (facets, Cmd.none)
     in
     ( { model with facets }, cmd)
  | ToggleFacetItem (_,_) ->
     let model = toggleFacetMenu ~mode:false ~model in
     ( model, Cmd.none)
  | GetFacets _-> (model, Cmd.none)

let isFacetActive ~filters ~facetKey ~facetValue =
  List.exists
    (fun (key, value) -> (facetKey = key && facetValue = value))
    filters

let getFacetLabel ~key ~value ~type' ~translations =
    match type' with
      | FacetNormal -> Util.trans value translations
      | FacetBoolean ->
         let key = Printf.sprintf "%s_%s" key value in
         Util.trans key translations
  
let facetList ~facets ~filters ~context =
  let renderFacetItem ~key ~type' ~(item:Types.facetItem) ~filters =
    let isActive =
      isFacetActive ~filters ~facetKey:key ~facetValue:item.value
    in
    let label = getFacetLabel
                  ~key
                  ~value:item.value
                  ~type'
                  ~translations:context.translations
    in
    let label = Printf.sprintf "%s: (%s)" label (Util.toLocaleString item.count) in
    li [
        onClick (ToggleFacetItem ((not isActive), ( key, item.value )))
      ; class' (Style.facetItem isActive)
      ]
      [
        h3
          [ class' (Style.facetLabel ~active:isActive)]
          [ text label ]
      ]
  in  
  let renderFacetItems  ~key ~type' ~items ~filters =
    Array.map (fun item -> renderFacetItem ~key ~type' ~item ~filters) items
  in
  let facet ~f ~context =
    let renderFacet ~opened ~key ~items ~loading ~filters =      
      let icon = match (loading, opened) with
        | (true, _) -> Style.spinnerIcon
        | (_, true) -> Style.arrowIcon Style.ArrowDown
        | (_, false) -> Style.arrowIcon Style.ArrowRight
      in
      li [ class' (Style.facet ~opened ~loading)
         ; onClick (ToggleFacet key) ]
        [
          span [ class' icon ] []
        ; h2
            [ class' Style.facetTitle ]
            [ text (Util.trans key context.translations) ]
        ; (if opened then
             ul [ class' Style.facetItemsContainer ]
               (Array.to_list
                  (renderFacetItems ~key ~type':f.type' ~items ~filters))
           else
             noNode)
        ]
    in
    match f.items with
    | Success t -> renderFacet ~opened:f.opened ~key:f.key ~items:t ~loading:false ~filters
    | NotAskedType t -> renderFacet ~opened:f.opened ~key:f.key ~items:t ~loading:false ~filters
    | LoadingType t -> renderFacet ~opened:f.opened ~key:f.key ~items:t ~loading:true ~filters
    | _ -> noNode
  in
  ul [ ]
    (let keys = Js.Dict.keys facets in
     List.map (fun key ->
         match Js.Dict.get facets key with
         | Some f -> facet ~f ~context 
         | None -> noNode
       ) ( Array.to_list keys )
    )
  
let view ~model ~context ~filters =
  if model.isOpen = false then noNode else
    div [ class' Style.facetModal ]
      [
        div [ class' Style.facetHeader ] [
            h1 [ class' Style.facetHeading ]
              [ text (Util.trans "Narrow search" context.translations) ]
           ; a [ class' Style.closeFacetsIcon
              ; onClick CloseFacets ] []
          ]
      ; facetList ~facets:model.facets ~filters ~context
      ]
