open Css
   
let basePadding = em 1.0
let textColor = darkgrey
let greyLight = hex "dae1e7"
let greyLighter = hex "f1f5f8"
let yellowLight = (hex "ffffb1")
let btnBackground = yellowLight

let init =
  global "body" [ margin (px 0) ];
  global "html" [ display `block; fontFamily "sans-serif"; color black ];
  global "h1, h2, h3" [ color black ];
  global
    "h1, h2, h3, p"
    [ margin2 ~h: (px 0) ~v: (em 0.2); lineHeight (em 1.2)];
  global "a" [ textDecoration `none ];
  
  global "h1" [ fontWeight 400; fontSize (em 1.4) ];
  global "h2" [ fontWeight 400; fontSize (em 1.3) ];
  global "h3" [ fontWeight 300; fontSize (em 1.2) ];
  global "p" [ fontWeight 300; fontSize (em 1.0) ];
  global "a" [
      color (hex "005999")
      ]
  ;
  global 
    "ul, li"
    [ padding (px 0); margin (px 0); listStyleType `none ];
                                                        
  global "img"  [ maxWidth (pct 100.0); height auto ];
  global 
    "input"
    [
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
      position `absolute
    ; backgroundColor (hex "29ff29")
    ; padding (px 3)
    ; zIndex 20
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
  let frames =
    keyframes [
        (0, [ transform (rotate (`deg 0)) ] )
      ; (100, [ transform (rotate (`deg 360)) ] )
      ]
  in
        
  style [
      cursor `pointer
    ; width (px 40)
    ; height (px 40)
    ; backgroundImage (url "icons/spinner-solid.svg")
    ; display `inlineBlock
    ; verticalAlign `middle
    ; animation ~duration: 2000 ~timingFunction: `linear ~iterationCount: `infinite frames
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
      margin basePadding;
      backgroundColor bkgColor
    ]

let info = statusStyle ~error: false 
let error = statusStyle ~error: true

let searchBoxInputWrapper =
  style [
      overflow `hidden
    ]

let searchBox =
  style [
      fontSize (em 1.5)
    ; height (em 2.5)
    ; backgroundColor white
    ; borderRadius (px 4)
    ; width (pct 100.0)
    ]

let searchBoxWrapper =
  style [
    padding basePadding
    (* ; borderBottom (px 2) `solid black *)
    ; position `sticky
    ; top (px 0)
    ; backgroundColor yellow
    ]

let searchBoxSubmit ~active =
  style 
      (List.append
        [
        float `right
        ; marginLeft (em 0.5)
        ; marginTop (em 0.4)
        ; fontSize (em 1.3)
        ; width (px 40)
        ; height (px 40)
        ; backgroundImage (url "icons/search-solid.svg")
        ; backgroundRepeat `noRepeat
        
        ] (if active = true then 
             [ cursor `pointer ]
           else
             [] )
      )
  
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
    ; backgroundColor yellow
    ]

let facetHeading =
  style [
      display `inlineBlock
    ]

let facets =
  style [
    backgroundColor greyLighter;
    borderBottom (px 1) solid greyLight
    ]

let facetMenu =
  style [
      padding2 ~h: (px 0) ~v: (em 0.2)
    ]

let facetItemsContainer =
  style [
      padding2 ~h:basePadding ~v:(px 0)
    ; paddingLeft (em 1.5)
    ; backgroundColor white
    ]

let facetTitleContainer =
  style [
      padding basePadding
    ; position `sticky
    ; top (px 0)
    ; backgroundColor yellowLight
    ; zIndex 10
    ]
  
let facetTitle ~active =
  style [
      display `inlineBlock
    ; marginLeft (em 0.5)
    ; fontWeight (if active then 700 else 500)
    ; fontStyle (if active then `italic else `normal)
    ]

let facetTitleCount =
  style [
      position `absolute
    ; right (em 0.5)
    ]
  
let facetItem =
  style [
      padding2 ~v:(em 1.0) ~h:(px 0)
    ; cursor `pointer
    ]

let facetLabel ~active =
  if active then
    style [
        fontWeight 700
      ; fontStyle `italic
      ]
  else
    style []

let facetItemLabelCount =
  style [
      position `absolute
    ; right (em 0.5)
    ]
  
let facet ~opened ~loading ~hasFacets =
  let disabled =
    if opened && (not hasFacets) && (not loading) then true else false
  in
  style [
      backgroundColor (if opened = true then yellowLight else white)
    ; borderBottom (px 1) `solid black
    ; cursor (if disabled then `default else `pointer)
    ; opacity (if disabled then 0.5 else 1.0)
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
  let borderCol = match (visited,lastVisited) with
    | (_,true) -> yellow
    | (true,_) -> hex "a7abc5"
    | _ -> white
  in
  style [
      borderBottom (px 1) solid greyLight
    ; borderLeft (px 4) solid borderCol
    ; padding2 ~v: (em 0.5) ~h: (em 1.0)
    ; color black
    ; hover [ backgroundColor greyLighter]
    ; cursor `pointer
    ]

let recordRowPadding = (em 0.5)
let recordRow = style [ marginTop recordRowPadding ]
let recordFieldRow = style [ marginTop (em 1.0) ]

let recordFormat =
  style [
    display `inlineBlock;
    padding2 ~v: (em 0.1) ~h: (em 0.5);
    backgroundColor greyLight;
    marginRight (em 0.5)
    ]

let recordListFacetLinks = style [ marginBottom (em 0.5) ]

let recordContent =
  style [
      padding basePadding
    ]

let recordLinks =
  style [
      marginTop (em 1.0)
    ; backgroundColor (hex "dadada")
    ; padding (em 0.5)
    ; lineHeight (em 1.4)
    ]
  
let recordLink =
  style [
      wordBreak `breakAll
        ; whiteSpace `nowrap
        ; overflow `hidden
        ; textOverflow `ellipsis
    ]

let recordImages = style [ marginTop (em 1.0) ]

let recordImageContainer =
  style [
    ]

let recordImageSpinner =
  style [
    width (px 40)
    ; height (px 40)
    ; backgroundImage (url "icons/spinner-solid.svg")
    ; marginLeft (`calc (`sub, (pct 50.0), (px 40)))
    ; marginBottom (`calc (`sub, (px 0), (px 55)))
    ; paddingTop (px 20)
    ; backgroundRepeat `noRepeat
    ; height (px 50)
    ]
  
let recordImage ~loading =
  style (
      List.append
        [
          width (pct 100.0)
        ; minHeight (px 200)
        ]        
        (if loading = true then
           [ border (px 1) `solid greyLight ]
         else
           []
        )    
    )

let recordAuthors =
  style [
      fontWeight 400
    ; marginRight (em 0.5)
    ; marginTop recordRowPadding
    ]

let recordFieldHeader =
  style [
      fontWeight 700 
    ]
  
let recordFieldText =
  style []

let recordSearchLinksContainer =
  style [
      margin2 ~v: (em 1.0) ~h:(px 0)
    ]

let recordSearchLinkList =
  style [
      lineHeight (em 1.4)
    ]
  
let recordSearchLink =
  style [
      cursor `pointer
    ]
  
let recordPublisher = style [ marginRight (em 0.3); fontSize (em 1.0) ]
let recordIsbn = style [ marginRight (em 0.3); fontSize (em 1.0) ]
let recordYear = style [ fontSize (em 1.0) ]
let recordPublished = style [ marginRight (em 0.5) ]
let recordSummary = style [ ]
let recordFinnaLink =
  style [
      color blue
    ; marginTop (em 1.0)
    ]
                  
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
    ; position `sticky
    ; top (px 0)
    ; padding2 ~v:(em 0.5) ~h:basePadding
    ; borderBottom (px 1) `solid (hex "d6d6d6")
    ; padding2 ~h:(px 0) ~v:(em 0.5)
    ; backgroundColor yellow
    ]
  
let paginateInfo =
  style [
      display `inlineBlock
    ; fontSize (em 1.2)
    ; marginLeft (em 1.0)
    ; marginRight (em 1.0)
    ; verticalAlign `middle
    ; padding2 ~h:(em 0.5) ~v:(px 0)
    ]

let closeIcon =
  [
      cursor `pointer
    ; width (px 40)
    ; height (px 40)
    ; backgroundRepeat `noRepeat
    ; backgroundImage (url "icons/times-circle-regular.svg")
    ]

let searchIcon =
  [
      cursor `pointer
    ; width (px 40)
    ; height (px 40)
    ; backgroundRepeat `noRepeat
    ; backgroundImage (url "icons/search-solid.svg")
    ]

  
let closeFacetsIcon =
  style (List.append closeIcon [float `right])

let closeRecordIcon = closeFacetsIcon 

let filterTools =
  style [
      overflow `hidden
    ; padding2 ~h:basePadding ~v:(em 0.5) 
    ; borderBottom (px 2) `solid black
    ; backgroundColor yellowLight 
    ]
  
let filterContainer =
  style [
      float `left
    ]

let filterType =
  style [ fontWeight 700 ]

let filterLabel = style []
                      
let removeFilter =
  style [
      cursor `pointer
    ]

let removeFilterIcon =
  style (List.append closeIcon [ display `inlineBlock ])

let removeFilterLabel =
  style [
      display `inlineBlock
    ; marginLeft (px 10)
    ]
  
let openFacets ~active =
  style (List.append [
             float `left
           ; marginRight (em 1.0)
           ; marginBottom (em 0.5)
           ] (if active then
                [ cursor `pointer ]
              else
                [ opacity 0.6])
    )

   
let facetsIcon =
  style [
      height (px 40)
    ; color black
    ; backgroundImage (url "icons/cog-solid.svg")
    ; display `inlineBlock
    ; verticalAlign `middle
    ; backgroundRepeat `noRepeat
    ]

let facetsIconLabel =
  style [
      display `inlineBlock
    ; fontSize (em 1.2)
    ; marginLeft (px 55) 
    ; marginTop (px 8)
    ; verticalAlign `middle
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
    "icons/arrow-alt-circle-" ^ file ^ "-regular.svg"
  in
  style [

     width (px 40)
    ; height (px 40)
    ; backgroundImage (url file)
    ; display `inlineBlock
    ; verticalAlign `middle
    ]

let paginateButton dir =
  Printf.sprintf "%s %s" (style [ cursor `pointer ]) (arrowIcon dir)
  
  

let nextPage ~loading =
  style (List.append
    [
        fontSize (em 1.5)
      ; textAlign `center
      ; padding (em 0.5)
      ; margin (em 1.3)
      ; backgroundColor btnBackground
      ; border (px 2) `solid black
      ; borderRadius (em 0.2)
    ] (if loading then [] else [
           cursor `pointer
    ])
    )

let nextPageLabel =
  style [
      marginRight (em 0.5)
    ]

let textLink =
  style [
      before [
          contentRule("> ")
        ]
    ]

