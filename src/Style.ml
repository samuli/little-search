open Css
   
let basePadding = em 1.0
let textColor = darkgrey
let greyLight = hex "dae1e7"
let greyLighter = hex "f1f5f8"

let init =
  global "body" [ margin (px 0) ];
  global "html" [ display `block; fontFamily "sans-serif"; color black ];
  global "h1, h2, h3" [ color black ];
  global
    "h1, h2, h3, p"
    [ margin2 ~h: (px 0) ~v: (em 0.2); lineHeight (em 1.2)];
  global "a" [ textDecoration `none ];
  
  global "h1" [ fontWeight 400; fontSize (em 1.5) ];
  global "h2" [ fontWeight 400; fontSize (em 1.3) ];
  global "h3" [ fontWeight 300; fontSize (em 1.3) ];
  global "p" [ fontWeight 300; fontSize (em 1.0) ];
  global "a" [ color black ];
  global 
    "ul, li"
    [ padding (px 0); margin (px 0); listStyleType `none ];
                                                        
  global "img"  [ maxWidth (pct 100.0); height auto ];
  global 
    "input"
    [
      boxSizing borderBox;
      maxWidth (pct 100.0);
      minWidth (pct 100.0);
      width (pct 100.0);
      padding (rem 0.5);
      fontSize (pct 100.0);
      borderRadius (em 0.2);
      borderWidth (px 1);
      borderColor (hex "d8d8d8");
      placeholder [ opacity 50.0 ]
    ]

let loadingIndicator ~show =
  let base =
    [
        position `absolute;
        backgroundColor yellow;
        padding (px 3)
      ]
  in
                 
  let display =
    if show then
      [ 
        opacity 1.0;
        (* transition ~delay: 500 ~duration: 100 "opacity" *)
      ]
    else
      [ 
        opacity 0.0;

      ]
  in
  style (List.append base display)

        
let spinnerIcon =
  (* let frames =
   *   keyframes [
   *       (0, [ rotate (`deg 0) ] )
   *     ; (100, [ rotate (`deg 360) ] )
   *     ]
   * in *)
        
  style [
      cursor `pointer
    ; width (px 40)
    ; height (px 40)
    ; backgroundImage (url "/icons/spinner-solid.svg")
    ; display `inlineBlock
    ; verticalAlign `middle
    (* ; animation ~duration: 2000 ~timingFunction: `linear ~iterationCount: `infinite frames *)
    ]
  
let pageYScroll ~allow =
  let body = Web_document.body() in
  let css = "preventYScroll" in
  if not allow then
    Web.Node.setAttribute body "class" css
  else
    Web.Node.removeAttribute body "class"

let statusStyle ~error =
  let bkgColor = if error then red else white in
  style [
      padding basePadding;
      margin2 ~h: (px 0) ~v: basePadding;
      backgroundColor bkgColor
    ]

let info = statusStyle ~error: false 
let error = statusStyle ~error: true
         
let searchBox =
  style [
      fontSize (em 1.5)
    ; backgroundColor white
    ; borderRadius (px 4)
    ]

let searchBoxWrapper =
  style [
    padding basePadding
    ; borderBottom (px 2) `solid black    
    ]

let searchBoxSubmit =
  style [
      border (px 2) `solid black
    ; padding (em 0.8)
    ; margin2 ~v:(em 1.0) ~h:(px 0)
    ; borderRadius (em 0.5)
    ; fontSize (em 1.3)
    ; backgroundColor (hex "d6d6d6")
    ; cursor `pointer
    ]
  
let facetModal =
  style [
      position `fixed
    ; top (px 0)
    ; width (`percent 100.0)
    ; height (`percent 100.0)
    ; backgroundColor white
    ; overflowY `scroll
    ]

let facetHeader =
  style [
      overflow `hidden
    ; padding2 ~v:(em 1.0) ~h:basePadding
    ; borderBottom (px 1) `solid (hex "d6d6d6")
    ; backgroundColor greyLighter
    ]

let facetHeading =
  style [
      display `inlineBlock
    ]

let facets =
  style [
    backgroundColor greyLighter;
    padding basePadding;
    borderBottom (px 1) solid greyLight
    ]

let facetMenu = style [ padding2 ~h: (px 0) ~v: (em 0.2) ]

let facetItemsContainer =
  style [
      marginLeft (em 1.0)
    ]

let facetTitle =
  style [
      display `inlineBlock
    ; marginLeft (em 0.5)
    ]
  
let facetItem _active =
  style [
      padding2 ~v:(em 1.0) ~h:(px 0)
    ; cursor `pointer
    ]
    
  (* match active with
   *   | true -> style [ border (px 2) `solid red ]
   *   | false -> style [] *)

let facet ~opened ~loading =
  let col = if opened = true then yellow else white in

  style [
      backgroundColor col
    ; padding basePadding
    ; borderBottom (px 1) `solid black
    ; cursor `pointer
    ]
  
let facetLink =
  style [
    display `inlineBlock
    ; fontSize (em 0.9)
    ; padding2 ~v: (em 0.1) ~h: (em 0.5)
    ; backgroundColor greyLight
    ]

let searchResults = style []
let searchResultsInfo = style [ padding basePadding; fontWeight 800 ]

let container = style []
let pad = style [ padding basePadding ]


let recordList ~_visited =
  style [
      borderBottom (px 1) solid greyLight;
    ]

let recordListBkg ~visited ~lastVisited =
  (* let bkgColor = if visited then hex "eff8ff" else white in *)
  let borderCol = match (visited,lastVisited) with
    | (_,true) -> blue
    | (true,_) -> hex "ef447b"
    | _ -> white
  in
  style [
      borderBottom (px 1) solid greyLight;
      borderLeft (px 4) solid borderCol;
      padding2 ~v: (em 0.5) ~h: (em 1.0);
      (* backgroundColor bkgColor; *)
      hover [ backgroundColor greyLighter];
      cursor `pointer
    ]

let recordFormat =
  style [
    display `inlineBlock;
    fontSize (em 1.0);
    padding2 ~v: (em 0.1) ~h: (em 0.5);
    backgroundColor yellow;
    marginRight (em 0.5)
    ]

let recordListFacetLinks = style [ marginBottom (em 0.5) ]

let recordContent =
  style [
      padding basePadding
    ]

let recordLinks = style [ marginLeft (em 1.0); listStyleType `circle ]
let recordLink = style [ listStyleType `disc; wordBreak `breakAll ]

let recordImages = style [ marginTop (em 1.0) ]
let recordImage =
  style [
    marginTop (em 0.5);
    minHeight (px 200);
    border (px 1) `solid greyLight;
    ]
let recordAuthors = style [ fontWeight 400; marginRight (em 0.5) ]
let recordPublisher = style [ marginRight (em 0.3); fontSize (em 1.0) ]
let recordYear = style [ fontSize (em 1.0) ]
let recordPublished = style [ marginRight (em 0.5) ]
let recordSummary = style [ fontStyle `italic ]
let recordFinnaLink = style [ color blue ]
                  
let languageMenuContainer =
  style [
      padding2 ~v:(em 1.0) ~h:basePadding
    ; borderTop (px 1) `solid (hex "d6d6d6")
    ]
  
let languageMenu =
  style [
      overflow `hidden
    
    ]
  
let language ~active =
  style [    
      fontSize (em 0.9)
    ; cursor `pointer
    ; padding2 ~v: (em 0.0) ~h: (em 0.5)
    ; float `left
    ; marginRight (em 0.5)
    ; fontWeight (if active then 800 else 300)
    ]

let paginationContainer =
  style [
      overflow `hidden
    ; padding2 ~v:(em 0.5) ~h:basePadding
    ; borderBottom (px 1) `solid (hex "d6d6d6")
    ]
  
let paginateInfo =
  style [
      display `inlineBlock
    ; fontSize (em 1.2)
    ; marginLeft (em 1.0)
    ; marginRight (em 1.0)
    ; verticalAlign `middle
    ; padding2 ~h:(em 1.0) ~v:(em 0.5)
    ]
  
let closeIcon =
  style [
      cursor `pointer
    ; float `right
    ; width (px 40)
    ; height (px 40)
    ; backgroundImage (url "/icons/times-circle-regular.svg")
    ]

type arrowDir =
  | ArrowUp | ArrowRight | ArrowDown | ArrowLeft
                      
let arrowIcon (dir:arrowDir) =
  let file = match dir with
    | ArrowUp -> "up"
    | ArrowRight -> "right"
    | ArrowDown -> "down"
    | ArrowLeft -> "left"
  in
  let file =
    "/icons/arrow-alt-circle-" ^ file ^ "-regular.svg"
  in
  style [
      cursor `pointer
    ; width (px 40)
    ; height (px 40)
    ; backgroundImage (url file)
    ; display `inlineBlock
    ; verticalAlign `middle
    ]

let nextPage ~loading =
  style ([
        fontSize (em 1.5)
      ; textAlign `center
      ; padding (em 0.5)
      ; margin (em 1.3)
      ; backgroundColor greyLight
      ; borderRadius (rem 0.5)] @
           (if loading then [] else
              [ 
                cursor `pointer
    ]))

let nextPageLabel =
  style [
      marginRight (em 0.5)
    ]

