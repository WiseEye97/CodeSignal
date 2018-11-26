

type Node = {
    Left : Option<Node>
    Right : Option<Node>
    V : int
}

let heightOfBST values =
    let rec addToBst node i num = 
        match node with
        | Some n ->
                    let v = n.V
                    match num - v with
                    | x when x > 0 -> let right,r = (addToBst n.Right (i + 1) num)
                                      Some {n with Right = right},r
                    | _ -> let left,r = (addToBst n.Left (i + 1) num) 
                           Some {n with Left = left},r
        | _ -> (Some {Left = None ; Right = None; V = num}),i
    match values with
    | [||] -> 0
    | [|_|] -> 1
    | x -> x |> Array.fold (fun s e -> 
                            let t,i = addToBst (s |> fst) 1 e
                            t , (s |> snd |> max i)
                           ) (None,0)
             |> snd
   
    

heightOfBST [|34; 96; -73; 10; -98; 25|]