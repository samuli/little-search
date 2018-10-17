open Types
   
let urlToRoute location : Types.route =
  let route = Js.String.split "/" location.Web.Location.hash in
  match route with
  | [|"#"; "Search"; query|] ->
     let query = Js.String.substr ~from:1 query in
     let params = Js.String.split "&" query |> Array.to_list in

     let params = List.map (fun param ->
                      match Js.String.split "=" param with
                      | [| key; value |] -> (key, value)
                      | _ -> ("", "")) params in

     let (_, lookfor) =
       match List.find (fun (key, _value) -> key = "lookfor") params with
       | param -> param
       | exception Not_found -> ("","")
     in
     
     (* let lookfor = Js.String.split "=" (List.hd params) in
      * let params = List.tl params in *)
     SearchRoute (lookfor, [])
  | [|"#"; "Record"; recordId|] -> RecordRoute recordId
  | _ -> MainRoute

let routeToUrl = function
  | SearchRoute (lookfor, _params) -> Printf.sprintf "#/Search/?lookfor=%s" lookfor
  | RecordRoute recordId -> Printf.sprintf "#/Record/%s" recordId
  | _ -> "#"

let openUrl url = Tea.Navigation.newUrl url

let openRoute route =
  openUrl (routeToUrl route)
  
