
type recordId = string
              
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
       
          
type context = {
    language: language; 
    translations: string Js.Dict.t remoteData;
    recordIds: string list;
  }

type contextUpdate =
  | NoUpdate
  | UpdateTranslations of string Js.Dict.t remoteData
  | UpdateRecordIds of string list
[@@bs.deriving {accessors}]
             
type route =
  | MainRoute
  | SearchRoute of searchParams
  | RecordRoute of recordId


type page =
  | PageReady of route
  | PageLoading of route

(* type filterType = {
 *     key: string;
 *     value: string
 *   } *)

(* type searchParamsType = {
 *     lookfor: string;
 *     limit: int;
 *     page: int;
 *     filters: searchParam array;
 *   } *)

                          
