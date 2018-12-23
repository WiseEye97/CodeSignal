open System.Collections.Generic
open System.Linq

type Story = {
    id : int
    score : int
    height : int
    stamp : int
}

type Pos = {
    row : int
    col : int
}

type Eq = 
    | First
    | Second

let feedOptimizer span h events =
    
    let rec cmpList = function
        | [],h::t -> First
        | h::t,[] -> Second
        | h1::t1,h2::t2 when h2 < h1 -> Second
        | h1::t1,h2::t2 when h2 > h1 -> First
        | h1::t1,h2::t2 -> cmpList(t1,t2)
        | _ -> failwith "list are equal LOL"

    let rec fetchRes pos res (arr:int [] []) (arr2:Story []) =
        match pos with
        | {row = r;col = c} when r = 0 -> if arr.[r].[c] > 0 then arr2.[r].id :: res else res  
        | {row = r;col = c} -> let upper = arr.[r - 1].[c]
                               let upperLeft = if c - arr2.[r].height >= 0 then arr.[r - 1].[c - arr2.[r].height] else -1
                               
                               if upper = arr.[r].[c] then
                                if upperLeft + arr2.[r].score = arr.[r].[c] && upperLeft <> -1 then
                                    let another = fetchRes {pos with row = r - 1;col = c - arr2.[r].height} (arr2.[r].id :: res) arr arr2
                                    let another1 = fetchRes {pos with row = r - 1} res arr arr2
                                    let d = another1.Length - another.Length
                                    if d = 0 then
                                        match cmpList(another,another1) with
                                        | First -> another
                                        | _ -> another1
                                    else
                                        match d with
                                        | x when x < 0 -> another1
                                        | _ -> another
                                else
                                    fetchRes {pos with row = r - 1} res arr arr2
                               else   
                                fetchRes {pos with row = r - 1;col = c - arr2.[r].height} (arr2.[r].id :: res) arr arr2

    let findBest h (stories:Queue<Story>) =
        if stories.Count = 0 then
            [[0]]
        else
            let arr = 
                stories.ToArray()
                |> Array.sortBy (fun st -> st.height)

            let dp = [|for _ in 1..arr.Length -> [|for _ in 0..h -> 0|]|]

            let fw = arr.[0].height

            for i in 1..h do 
                if fw <= i then dp.[0].[i] <- arr.[0].score 

            for i in 1..(arr.Length - 1) do
                let v,w = arr.[i].score , arr.[i].height
                for j in 1..h do
                    dp.[i].[j] <- if j >= w then max (v + dp.[i - 1].[j - w]) dp.[i - 1].[j] else dp.[i - 1].[j]
            if dp.[arr.Length - 1].[h] = 0 then
                [[0]]
            else
                fetchRes {row = arr.Length - 1 ; col = h} [] dp arr  
                |> List.sort
                |> (fun li -> [[dp.[arr.Length - 1].[h]]@li])

    let rec clearStories (stories:Queue<Story>) now =
        if stories.Count = 0 then
            ()
        else
            now - stories.Peek().stamp
            |> (fun dif ->
                if dif > span then
                    stories.Dequeue() |> ignore
                    clearStories stories now
                else
                    ()
            )


    let rec traverse (stories:Queue<Story>) res i li =
        match li with
        | [] -> res
        | ([|stamp|])::t -> 
                            stamp |> clearStories stories
                            let r = stories |> findBest h |> List.append res 
                            traverse stories r i t

        | ([|stamp;score;height|])::t -> 
                                        let story = {id = i + 1;score = score;height = height;stamp = stamp}
                                        stories.Enqueue(story)   
                                        traverse stories res (i + 1) t
        | _ -> failwith "wrong arg"

    events
    |> List.ofArray
    |> traverse (new Queue<Story>()) [] 0
    |> List.map Array.ofList
    |> Array.ofList 

    
    
                
[<EntryPoint>]
let main _ =
    printfn " r -> %A" <| feedOptimizer 5 100 [|[|2|]; 
 [|3;10;50|]; 
 [|4;11;50|]; 
 [|9|]; 
 [|10|]|]
    0