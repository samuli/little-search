type recordId = string
type resultpageNum = int
             
type searchLookfor = string
type searchParam = (string * string)
type searchParams = {
    lookfor: string;
    filters: searchParam list;
    page: int;
    limit: int;
  }
                  
type 't remoteData =
  | NotAsked
  | NotAskedType of 't
  | Loading
  | LoadingType of 't
  | Error of string
  | Success of 't

type language =
  | LngFi
  | LngEn
  
let languageCode lng =
  match lng with
  | LngFi -> "fi"
  | LngEn -> "en"

let languageOfCode code =
  match code with
  | "en" -> LngEn
  | _ -> LngFi

let finnaLanguageCode lng =
  match lng with
  | LngFi -> "fi"
  | LngEn -> "en-gb"

            
type onlineUrl = {
  url: string option;
  label: string option;
}

type facetType = | FacetNormal | FacetBoolean
                 
type facetItem = {
    value: string;
    translated: string;
    count: int
  }

type translated = {
  value: string;
  translated: string
  }
                
type record = {
  id: string;
  title: string option;
  formats: translated array option;
  images: string array option;
  authors: string array option;
  buildings: translated array option;
  publishers: string array option;
  year: string option;
  onlineUrls: onlineUrl array option;
  urls: onlineUrl array option;
  summary: string array option;
  }

type searchResult = {
  records: record array;
  resultCount: int;
  }

type searchResultPageType = {
    page: int;
    results: searchResult remoteData;
  }
type searchResultsType = {
    count: int;
    pageCount: int;
    pages: searchResultPageType Js.Dict.t;
  }
                       
type paginationCmd =
  | PaginateNoCmd
  | PaginateRecordCmd of recordId
  | PaginatePrevCmd of (int * recordId)
  | PaginateNextCmd of (int * recordId)
[@@bs.deriving {accessors}]

type paginationItem = {
    id: recordId;
    next: paginationCmd;
    prev: paginationCmd;
    pageNum: int;
    ind: int;
}

type pagination = {
    items: paginationItem list;
    count: int;
    limit: int;
}

type route =
  | MainRoute
  | SearchRoute of searchParams
  | RecordRoute of recordId

type context = {
    language: language; 
    translations: string Js.Dict.t remoteData;
    prevRoute: route option;
    pagination: pagination
  }

type navigateDir =
  | Forward
  | Backward

type navigateCmd =
  | NoNavigate
  | Navigate of (recordId * navigateDir)

                 
type contextUpdate =
  | NoUpdate
  | UpdateTranslations of string Js.Dict.t remoteData
  | UpdatePagination of pagination
  | LoadResultsInBackground of resultpageNum
  | GotResultsInBackground
  | PageLoaded of route 
[@@bs.deriving {accessors}]

               
type page =
  | PageReady of route
  | PageLoading of route

                          
