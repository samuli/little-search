
type recordId = string
              
type searchLookfor = string
type searchParam = (string * string)
type searchParams = (searchParam list)

type 't remoteData =
  | NotAsked
  | NotAskedType of 't
  | Loading
  | LoadingType of 't
  | Error of string
  | Success of 't
                  
type context = {
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
  | SearchRoute of (string * searchParams)
  | RecordRoute of recordId


type page =
  | Ready of route
  | Loading of route

(* type filterType = {
 *     key: string;
 *     value: string
 *   } *)

type searchParamsType = {
    lookfor: string;
    limit: int;
    page: int;
    filters: searchParam array;
  }

                          
