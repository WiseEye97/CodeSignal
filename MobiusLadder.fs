//checking if given graph respresented as adj list is a Mobius Ladder
// https://en.wikipedia.org/wiki/M%C3%B6bius_ladder

let isMobiusLadder n ladder =
    //contructing a graph
    let addToMap key value map =
      if map |> Map.containsKey key then
         let t = map.[key]
         map 
         |> Map.remove key 
         |> Map.add key (t |> Set.add value)
      else
         map
         |> Map.add key (new Set<int>([value]))

    ladder
    |> Array.fold (fun state arr ->
         match arr with
         | [|n1;n2|] -> 
                        state
                        |> addToMap n1 n2
                        |> addToMap n2 n1
         | _ -> failwith "wrong arg"

    ) (new Map<int,Set<int>>([]))   //In order a graph to be a Mobius Ladder he has to match some properties :
    |> (fun map ->                  // all nodes must have exactly degree of 3
        (map |> Map.forall (fun _ v -> v.Count = 3)) && map.Count = n && n%2 = 0 , map //the number of nodes must be even
    )                                                                                  // there cannot be cycles of length in graph
    |> (fun (r,map) ->                                                                 // in only one case there must be cycle of len 3 when n = 4
        match r with
        | true when n = 4 -> true
        | true -> 
                  map
                  |> Map.forall (fun k v -> 
                    v |> Set.forall (fun n -> map.[n] |> Set.forall (fun x -> map.[x].Contains(k) |> not) )
                  )  
                  
        | _ -> false
    ) 
      
    
[<EntryPoint>]
let main _ =
    printfn "res -> %A" <| isMobiusLadder 8 [|[|0;1|]; 
 [|0;2|];
 [|0;7|]; 
 [|1;3|]; 
 [|1;6|]; 
 [|2;3|]; 
 [|2;4|]; 
 [|3;5|]; 
 [|4;5|]; 
 [|4;6|]; 
 [|5;7|]; 
 [|6;7|]|]
    0 // return an integer exit code
    

