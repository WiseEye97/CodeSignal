//https://app.codesignal.com/interview-practice/task/qmKLsQcqeEBckLx2q

open System.Collections.Generic
open System.Linq

type Edge = {node1 : int ; node2 : int}

let singlePointOfFailure connections =
    let l , l1 = Array.length connections , Array.length connections.[0]

    let isIn (i,j) = i >= 0 && i < l && j >= 0 && j < l1

    let move =
        function
        | i,j when j = l1 - 1 -> (i + 1 , 0)
        | i,j -> (i , j + 1)

    let graph = new Dictionary<int,List<int>>()
 
    for i in 1 .. l do graph.[i - 1] <- new List<int>()

    let createEdge i j =
        match i,j with
        | a , b when a < b -> {node1 = a ; node2 = b}
        | _ -> {node1 = j ; node2 = i}
    
    let rec dfs node removedEdge (visited:HashSet<int>) =
        let nodes = graph.[node].Where((fun e -> not (visited.Contains(e))))
        for n in nodes do
            let edge = createEdge node n
            if edge <> removedEdge then
                visited.Add(n) |> ignore
                dfs n removedEdge visited |> ignore
        visited.Count
       

    let rec travers i j edges =
        let ii,jj = move (i ,j)
        match i,j with
        | pos when isIn pos && connections.[i].[j] = 1 ->   
                                                          graph.[i].Add(j) 
                                                          if i < j then
                                                            travers ii jj (List.append edges [createEdge i j])
                                                          else
                                                            travers ii jj edges   
        | pos when isIn pos -> travers ii jj edges
        | _ -> edges

    let edges = travers 0 0 []

    List.sumBy (fun edge ->
         let r = dfs 0 edge (new HashSet<int>([0]))
         if r <> l then 1 else 0
    ) edges

