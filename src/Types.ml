
type recordId = string
type searchQuery = string
                 
type route =
  | Main
  | Search of searchQuery
  | Record of recordId

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

