type searchterm = string
type recordId = string
type resultpageNum = int
type resultLimit = int
type resultCount = int
                 
type searchLookfor = string
type searchParam = (string * string)
type searchParams = {
    lookfor: string;
    filters: searchParam list;
    page: resultpageNum;
    limit: resultLimit;
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
  isbn: string option;
  issn: string option;
  publishers: string array option;
  year: string option;
  publicationDates: string array option;
  onlineUrls: onlineUrl array option;
  urls: onlineUrl array option;
  summary: string array option;
  measurements: string array option;
  (* languages: string array option; *)
  genres: string array option;
  subjects: string array array option;
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

type paginationRecord = (int * recordId)
                    
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
    appPath: string;
    settings: string Js.Dict.t remoteData;
    language: language; 
    translations: string Js.Dict.t remoteData;
    visitedRecords: recordId array;
    prevRoute: route option;
    pagination: pagination;
    recordIds: paginationRecord list;
    numOfResults: resultCount;
    resultLimit: resultLimit;
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
  | UpdateResultInfo of (resultCount * paginationRecord list * resultLimit)
  | UpdateVisitedRecords of recordId array
  | LoadResultsInBackground of resultpageNum
  | GotResultsInBackground
  | PageLoaded of route
  | NewSearch of (searchterm option * searchParam option)
  | BackToSearch
[@@bs.deriving {accessors}]

               
type page =
  | PageReady of route
  | PageLoading of route

                          
