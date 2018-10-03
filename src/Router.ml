open Types
   
let urlToRoute location =
  let route = Js.String.split "/" location.Web.Location.hash in
  match route with
  | [|"#"; "Search"; query|] -> Search query
  | [|"#"; "Record"; recordId|] -> Record recordId
  | _ -> Main

let routeToUrl = function
  | Search query -> Printf.sprintf "#/Search/%s" query
  | Record recordId -> Printf.sprintf "#/Record/%s" recordId
  | _ -> "#"

let openUrl url = Tea.Navigation.newUrl url

let openRoute route =
  openUrl (routeToUrl route)
  
