open Types
   
let urlToRoute location : Types.route =
  let route = Js.String.split "?" location.Web.Location.hash in
  match route with
  | [| "#/Search/"; query |] ->
     let params = Js.String.split "&" query |> Array.to_list in
     let params = List.map (fun param ->
                      match Js.String.split "=" param with
                      | [| key; value |] -> (key, value)
                      | _ -> (param, "")) params in
     let (lookfor, filters, page, limit) =
       Util.extractSearchParams params
     in
     
     SearchRoute {
         lookfor; filters;
         page = (int_of_string page)-1;
         limit = (int_of_string limit)
       }
  | [|"#/Record/"; recordId|] -> RecordRoute recordId
  | _ -> MainRoute

let routeToUrl = function
  | SearchRoute searchParams ->
     let lookfor = searchParams.lookfor in
     let filters = searchParams.filters in
     let page = searchParams.page+1 in
     let limit = searchParams.limit in
     let filters =
       if List.length filters > 0 then
         let filters =
           List.map (fun (key,value) ->
               Printf.sprintf "filter[]=%s:%s" key value)
             searchParams.filters
         in
         "&" ^ (String.concat "&" filters)
       else
         ""
     in
     Printf.sprintf "#/Search/?lookfor=%s&page=%d&limit=%d%s"
       lookfor page limit filters
  | RecordRoute recordId -> Printf.sprintf "#/Record/?%s" recordId
  | _ -> "#"

let openUrl url = Tea.Navigation.newUrl url

let openRoute route =
  openUrl (routeToUrl route)
  
