open System.Collections.Generic

type DSNode(initial) = 
   let v = initial

   let mutable parent : DSNode option = None

   let mutable rank = 1

   member private this.GetP() = parent

   member this.GetV() = v

   member private this.GetRank() = rank
   member private this.FindId (node:DSNode) = 
      match node.GetP() with
      | Some x -> this.FindId x
      | _ -> node

   member this.GetId() = 
      this
      |> this.FindId
      |> (fun n -> 
         if n.GetV() <> v then
            parent <- Some n
         n.GetV(),n
      )
   
   member private this.App (other:DSNode) = parent <- Some other
   member this.merge (other:DSNode) = 
      match this.GetRank() , other.GetRank() with
      | a , b when a > b -> this |> other.App
      | a , b when a = b -> this |> other.App ; rank <- rank + 1
      | _ -> other |> this.App
      
let connectedNetwork n wires =

   let graph = Dictionary<int,(int*int) list>()

   let nds = [|for i in 1..n -> graph.[i] <- [] ;DSNode(i)|]

   let rejected,ini = 
      wires
      |> Array.sortBy (fun [|_;_;w|] -> w)
      |> Array.fold (fun (li,sm) [|n1;n2;w|] -> 
         let (id1,p1),(id2,p2) = nds.[n1 - 1].GetId() , nds.[n2 - 1].GetId()
         if id1 <> id2 then 
            p1.merge p2 
            graph.[n1] <- (n2,w) :: graph.[n1]
            graph.[n2] <- (n1,w) :: graph.[n2]
            li,(sm + w)
         else
            [n1;n2;w] :: li , sm
      ) ([],0)

   let rec dfs res prev node stp =

      let rec tryFind = function
         | [] -> -1
         | (n,_)::t when n = prev -> tryFind t
         | (n,w)::t ->  let r = dfs (if res = -1 || res < w then w else res) node n stp
                        if r <> - 1 then r else tryFind t

      match node with
      | x when x = stp -> res
      | _ when graph.[node].Length = 1 && prev <> -1  -> -1
      | _ -> tryFind graph.[node]

   rejected
   |> List.fold (fun state [n1;n2;w] ->
      dfs -1 -1 n1 n2 
      |> (function
            | -1 -> state
            | x -> min state (w - x)
      )
   )  System.Int32.MaxValue
 



    
                
[<EntryPoint>]
let main _ =
    connectedNetwork 4 [|[|1;2;1|]; [|1;4;3|]; [|2;3;3|]; [|2;4;2|]; [|3;4;4|]|]
    |> printfn "res -> %A"
    0