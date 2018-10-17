
type recordId = string
              
type searchLookfor = string
type searchParam = (string * string)
type searchParams = (searchLookfor * searchParam list)


type route =
  | MainRoute
  | SearchRoute of searchParams
  | RecordRoute of recordId

type 't remoteData =
  | NotAsked
  | NotAskedType of 't
  | Loading
  | LoadingType of 't
  | Error of string
  | Success of 't

type page =
  | Ready of route
  | Loading of route

type filterType = {
    key: string;
    value: string
  }

type searchParamsType = {
    lookfor: string;
    limit: int;
    page: int;
    filters: filterType array;
  }
