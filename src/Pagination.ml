open Types

let resultPageNums ~results =
  let pageNums = Js.Dict.keys results.pages in
  Array.sort (fun a b ->
      let a = int_of_string a in
      let b = int_of_string b in
      if a > b then 1 else -1) pageNums;
  pageNums

let recordNeighbors ~id ~ids ~limit ~resultCount =
  let rec find id ids cnt pageInd pageNum =
    match ids with
    | [] -> None
    | (recId, page) :: rest ->
       let pageInd = if pageNum <> page then 0 else pageInd in
       if recId = id then
         let recInd = (page*limit) + pageInd in
         Some (recId, page, recInd, cnt)
       else
         find id rest (cnt+1) (pageInd+1) page
  in
  match find id ids 0 0 999999999 with
  | Some (recId, pageNum, recInd, ind) -> begin
      let len = List.length ids in
      let prev = match recInd with
        | 0 -> PaginateNoCmd
        | _ ->
           if ind > 0 then
             let (prevId,_) = List.nth ids (ind-1) in
             paginateRecordCmd prevId
           else
             paginatePrevCmd (pageNum-1, recId) 
      in
      let next = match (len, recInd, len-ind) with
        | (_, b, _) when (resultCount-1) = b -> PaginateNoCmd (* last page *)
        | (_a, _, c) when c > 1 ->
           let (nextId, _) = List.nth ids (ind+1) in
           paginateRecordCmd nextId
        | _ -> paginateNextCmd (pageNum+1, recId)
      in
      Some { prev; next; pageNum; id; ind = recInd }
    end
  | _ -> None
  

let recordPages ~(results:searchResultsType) =
  let pageNums = resultPageNums ~results in

  let ids = List.map (fun pageNum->
        match Js.Dict.get results.pages pageNum with
        | Some page -> begin
            match page.results with
            | Success res ->
               let records = Array.to_list res.records in
               List.map (fun (r:record) -> (r.id, page.page)) records
            | _ -> []
          end
        | _ -> []) (Array.to_list pageNums)
  in
  (List.flatten ids, pageNums)

let paginateRecord ~id ~results ~limit =
  let (ids, _pageNums) = recordPages ~results in
  match List.find (fun (recId, _) -> recId = id) ids with
  | (recId,_) ->
     recordNeighbors
       ~id:recId ~ids ~limit
       ~resultCount:results.count
  | exception Not_found -> None

