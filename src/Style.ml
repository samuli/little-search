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
  
  global "h1" [ fontWeight 400; fontSize (em 1.3) ];
  global "h2" [ fontWeight 600; fontSize (em 1.0) ];
  global "p" [ fontWeight 300; fontSize (em 0.9) ];
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

(* let loadingIndicator ~show =
 *   let base = [
 *       display `block;
 *       width (`percent 100.0);
 *       height (px 4);
 *       margin (px 0);
 *       backgroundColor (rgba 0 0 0 0.34)
 *     ]
 *   in
 * 
 *   let display =
 *     if true then
 *       [ 
 *         opacity 1.0;
 *         transition ~delay: 500 ~duration: 100 "opacity"
 *       ]
 *     else
 *       [ 
 *         opacity 0.0;
 * 
 *       ]
 *   in
 *   style base
 *   style (List.append base display ) *)

  
(* let loadingBar =
 *   let frames = keyframes [
 *       (0, [ width (`percent 0.0) ] );
 *       (60, [ width (`percent 100.0); marginLeft (`percent 50.0) ] );
 *       (100, [ width (`percent 0.0); marginLeft (`percent 100.0) ] );
 *     ] in
 *   
 *   style [
 *       width (`percent 50.0);
 *       height (`percent 100.0);
 *       marginLeft (px 0);
 *       backgroundColor (hex "43b02A");
 *       animation ~duration: 750 ~timingFunction: `easeInOut ~direction: `alternateReverse ~iterationCount: `infinite frames
 *     ] *)
  
      
let statusStyle ~error =
  let bkgColor = if error then red else white in
  style [
      padding basePadding;
      margin2 ~h: (px 0) ~v: basePadding;
      backgroundColor bkgColor
    ]

let info = statusStyle ~error: false 
let error = statusStyle ~error: true
         
let nextPage ~loading =
  style ([
      textAlign `center;
      padding basePadding;
      margin basePadding;
      backgroundColor greyLight;
      borderRadius (rem 0.5)] @
    (if loading then [ 
      cursor `pointer;
      focus [ outlineStyle `none; borderColor blue ];
      hover [ backgroundColor (hex "cacdd0") ];
      ] else []))

let searchBox =
  style [
    backgroundColor white;
    borderRadius (px 4)
    ]

let searchBoxWrapper =
  style [
    padding basePadding;
    backgroundColor greyLight
    ]

let facetModal =
  style [
      position `fixed;
      top (px 0);
      width (`percent 100.0);
      height (`percent 100.0);
      backgroundColor green
    ]
  
let facets =
  style [
    backgroundColor greyLighter;
    padding basePadding;
    borderBottom (px 1) solid greyLight
    ]

let facetMenu = style [ padding2 ~h: (px 0) ~v: (em 0.2) ]

let facetItem active =
  match active with
    | true -> style [ border (px 2) `solid red ]
    | false -> style []

let facet mode =
  let col = match mode with
    | "loaded" -> yellow
    | "loading" -> red
    | _ -> white 
  in
  style [ backgroundColor col ]
  
let facetLink =
  style [
    display `inlineBlock;
    fontSize (em 0.9);
    padding2 ~v: (em 0.1) ~h: (em 0.5);
    backgroundColor greyLight
    ]

let searchResults = style []
let searchResultsInfo = style [ padding basePadding; fontWeight 800 ]

let container = style []
let pad = style [ padding basePadding ]


let recordList ~_visited =
  style [
      borderBottom (px 1) solid greyLight;
    ]

let recordListBkg ~visited =
  style [
      borderBottom (px 1) solid greyLight;      
      padding2 ~v: (em 0.5) ~h: (em 1.0);
      backgroundColor (if visited then hex "eff8ff" else white);
      hover [ backgroundColor greyLighter];
      cursor `pointer
    ]

let recordFormat =
  style [
    display `inlineBlock;
    fontSize (em 0.9);
    padding2 ~v: (em 0.1) ~h: (em 0.5);
    backgroundColor yellow;
    marginRight (em 0.5)
    ]

let recordListFacetLinks = style [ marginBottom (em 0.5) ]

let recordFull = style [ padding basePadding ]

let recordLinks = style [ marginLeft (em 1.0); listStyleType `circle ]
let recordLink = style [ listStyleType `disc; wordBreak `breakAll ]

let recordImages = style [ marginTop (em 1.0) ]
let recordImage =
  style [
    marginTop (em 0.5);
    minHeight (px 100);
    backgroundColor greyLight;
    ]
let recordAuthors = style [ fontWeight 400; marginRight (em 0.5) ]
let recordPublisher = style [ marginRight (em 0.3); fontSize (em 0.9) ]
let recordYear = style [ fontSize (em 0.9) ]
let recordPublished = style [ marginRight (em 0.5) ]
let recordSummary = style [ fontStyle `italic ]

