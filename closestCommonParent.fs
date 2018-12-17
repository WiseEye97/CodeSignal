open System.Collections.Generic

let closestCommonParent files parents file1 file2 =
    let graph =
        parents
        |> Array.zip files
        |> Map.ofArray
    
    let q = new Queue<string>([file1;file2])
    let v = new Set<string>([file1;file2])

    let rec bfs (q:Queue<string>) visited =
        if q.Count = 0 then
            "" 
        else
            let node = q.Dequeue()
            if node = "-1" then 
                bfs q visited
            else
                graph
                |> Map.find node
                |> (fun p ->
                    match visited |> Set.contains p with
                    | true -> p
                    | _ -> 
                           q.Enqueue(p)
                           visited |> Set.add p |> bfs q 
                )
            
    bfs q v

closestCommonParent [|"F1"; "F2"; "F3"; "F4"; "F5"; "F6"; "F7"; "F8"|]  [|"-1"; "F1"; "F1"; "F2"; "F2"; "F4"; "F4"; "F4"|] "F5" "F8"
