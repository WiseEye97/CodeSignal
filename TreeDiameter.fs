open System.Collections.Generic

//The task is to find a Diameter of the tree,it means you need to find the longest path beetween any nodes of a tree. 

let treeDiameter n tree =

   //Utility fun to add a value to map 
   //This map represents the graph
   let addToMap key value map =
      if map |> Map.containsKey key then
         let t = map.[key]
         map 
         |> Map.remove key 
         |> Map.add key (value::t)
      else
         map
         |> Map.add key ([value])
   //Recursive version of BFS to find the farest node from a given node
   let rec findFarest graph visited (q:Queue<int*int>) = 
       let node,cnt = q.Dequeue()
       graph
       |> Map.containsKey node
       |> (function 
         | true ->
                  graph.[node]
                  |> List.filter (fun e -> visited |> Set.contains e |> not)
                  |> List.fold (fun s e -> 
                     q.Enqueue((e,cnt + 1))
                     s |> Set.add e
                  ) visited
                  |> (fun s -> q,s)
         | _ -> q,visited
       )
       |> (fun (q,s) -> 
         if q.Count = 0 then
            node,cnt
         else
            findFarest graph s q
       )


   //making a graph from given edges
   let graph = 
      tree
      |> Array.fold (fun state arr ->
         match arr with
         | [|n1;n2|] -> 
                        state
                        |> addToMap n1 n2
                        |> addToMap n2 n1
         | _ -> failwith "wrong arg"

      ) (new Map<int,int list>([]))
   
   match tree with // covering some edge cases
   | [||] -> 0
   | [|_|] -> 1
   | _ -> 
           findFarest graph (new Set<int>([])) (new Queue<int*int>([0,0])) //running bfs twice , firstly from a random node then from the farest we found , second bfs gives the answer
           |> (fun (ini , _) -> findFarest graph (new Set<int>([])) (new Queue<int*int>([ini,0])) |> snd)
      
    
[<EntryPoint>]
let main _ =
    printfn "res -> %A" <| treeDiameter 10 [|[|2;5|]; 
 [|5;7|]; 
 [|5;1|]; 
 [|1;9|]; 
 [|1;0|]; 
 [|7;6|]; 
 [|6;3|]; 
 [|3;8|]; 
 [|8;4|]|]
    0 // return an integer exit code
