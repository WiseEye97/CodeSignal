(*
       Given scores, an array of integers representing all your test and assignment grades, 
       your task is to return an array of integers where output[i] represents the mean of all your scores up to index i.
*)
let meanScores scores =
        scores
        |> Array.fold (fun s e ->
                let ar,ar2,i = s
                ((if i > 0 then Array.get ar (i - 1) else 0) + e) |> Array.set ar i
                Array.set ar2 i (ar.[i] / (i + 1))
                ar , ar2 , i + 1 
        ) (Array.zeroCreate scores.Length , Array.zeroCreate scores.Length , 0)
        |> (function 
            | _,r,_ -> r
                )      

meanScores <| [|100; 20; 50; 70; 45|]  