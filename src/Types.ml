
type recordId = string
type searchQuery = string
                 
type route =
  | Main
  | Search of searchQuery
  | Record of recordId

type 't remoteData =
   | NotAsked
   | Loading
   | Error of string
   | Success of 't
