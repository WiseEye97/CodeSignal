open System.Collections.Generic

type Task = {
    startTime : float
    endTime : float
    tempo : float
    id : int
}

type PQ(capacity) = 
    let (arr:Task[]) = Array.zeroCreate capacity
    
    let mutable cnt = 0

    let entries = new Dictionary<int,int>()

    member this.Count() = cnt

    member this.GetParent i =
        match i%2 with
        | 0 -> i/2 - 1
        | _ -> i/2

    member this.Swap j i =
        let temp = arr.[i]
        arr.[i] <- arr.[j]
        arr.[j] <- temp
        entries.[arr.[j].id] <- j
        entries.[arr.[i].id] <- i
        i

    member this.CompareTask (t1:Task) (t2:Task) =
           match t1.endTime - t2.endTime with
           | 0.0 -> t1.startTime - t2.startTime
           | x -> x 

    member this.merge i =
        if i = 0 then
            ()
        else
            i
            |> this.GetParent
            |> (fun pi -> arr.[pi] ) 
            |> (fun (parent:Task) -> 
                let r = this.CompareTask arr.[i] parent
                match r with
                | x when x < 0.0 -> i |> this.GetParent |> this.Swap i |> this.merge
                | _ -> ()
            )

    member this.Add x = 
        arr.[cnt] <- x
        entries.[x.id] <- cnt
        cnt <- cnt + 1
        this.merge (cnt - 1)

    member this.GetHigherChild i = 
        let first,second = i*2 + 1 , i*2 + 2
        let task = arr.[i]
        match first,second with
        | a,b when a >= cnt && b >= cnt -> i
        | a,b when b >= cnt  -> 
                                let r = this.CompareTask task arr.[a]
                                match r with
                                | x when x > 0.0 -> first
                                | _ -> i
        | a,b -> 
                 let r1 = this.CompareTask arr.[a] arr.[b]
                 match r1 with
                 | x when x > 0.0 -> 
                                    let r = this.CompareTask task arr.[b]
                                    match r with
                                    | x when x > 0.0 -> second
                                    | _ -> i
                 | _ -> 
                        let r = this.CompareTask task arr.[a]
                        match r with
                        | x when x > 0.0 -> first
                        | _ -> i

    member this.Sink i =
        i 
        |> this.GetHigherChild
        |> (function
            | x when x = i -> ()
            | y -> 
                   this.Swap i y
                   |> this.Sink 
        )    

    member this.Modify (fn:(Task -> Task*float)) =
            for i in 0..(capacity - 1) do
                if entries.ContainsKey(i) then
                    let (t,r) = 
                        arr.[entries.[i]]
                        |> fn
                    t |> Array.set arr entries.[i]
                    match r with
                    | x when x >= 0.0 -> entries.[i] |> this.merge
                    | _ -> entries.[i] |> this.Sink
                                        
                    
           
    member this.Remove() = 
        let first = arr.[0]
        arr.[0] <- arr.[cnt - 1]
        cnt <- cnt - 1
        if cnt >= 1 then
            entries.[arr.[0].id] <- 0
        this.Sink 0 
        entries.Remove(first.id) |> ignore
        first


    member this.Peek() = arr.[0]
        

let backupTimeEstimator startTimes backupDuration maxThreads =

    let result = new Dictionary<int,float>()

    let q = new Queue<Task>()

    let initial = new Queue<Task>()

    startTimes
    |> Array.map float
    |> Array.zip (backupDuration |> Array.map float)
    |> Array.iteri (fun i (temp,st) ->
        let nst = st - (float <| startTimes.[0])
        initial.Enqueue({startTime = nst;endTime = nst + temp;tempo = 1.0/temp;id = i})
    )
   
     
    let calculateEndTime now (n : float) (dif:float) (task:Task)  =
        let cTempo = task.tempo/n
        let toGo = task.endTime - now
        let prev = task.endTime
        let ntoGo = if now = task.startTime && dif = -1.0 then toGo*(task.tempo/cTempo) else toGo*((task.tempo/(n + dif))/cTempo)
        let nxt = now + ntoGo
        {task with endTime = nxt},(prev - nxt)
    
    let createUpdater stamp n d = calculateEndTime stamp n d 

    let rec getFinished (pq:PQ) i stamp =
        if pq.Count() = 0 then
            ()
        else
            let top = pq.Peek()
            let et = top.endTime
            if et <= stamp then
                let x = pq.Remove()
                result.[x.id] <- et
                createUpdater et (pq.Count() |> float) (1.0) 
                |> pq.Modify
                getFinished pq (i + 1) stamp
            else
                ()
    
    let getNextStamp (pq:PQ) (initial:Queue<Task>) =
        match pq.Count() , initial.Count with
        | 0,0 -> None
        | 0,_ -> Some (initial.Peek().startTime)
        | _,0 -> Some (pq.Peek().endTime)
        | _,_ -> Some (min (initial.Peek().startTime) (pq.Peek().endTime))
    
    let rec movedQueuedTasks (pq:PQ) (apq:Queue<Task>) moment =
        match pq.Count() < maxThreads , apq.Count with
        | true,0 -> ()
        | true,_ -> let v = apq.Dequeue()
                    pq.Add({v with endTime = moment + (1.0/v.tempo); startTime = moment})
                    (createUpdater moment (pq.Count() |> float) (1 |> (fun x -> -1*x) |> float)) |> pq.Modify
                    movedQueuedTasks pq apq moment
        | _ -> ()
    
    let tryPush (pq:PQ) (apq:Queue<Task>) (initial:Queue<Task>) moment =
        let v = initial.Dequeue()
        if pq.Count() < maxThreads then
            pq.Add({v with endTime = moment + (1.0/v.tempo);startTime = moment})
            (createUpdater moment (pq.Count() |> float) (1 |> (fun x -> -1*x) |> float)) |> pq.Modify
        else
            apq.Enqueue(v)
        

    let rec travers (pq:PQ) (apq:Queue<Task>) (initial:Queue<Task>) moment =

        getFinished pq 0 moment
        
        match initial.Count with
        | 0 -> movedQueuedTasks pq apq moment
               if pq.Count() = 0 then
                    ()
               else
                    travers pq apq initial (pq.Peek().endTime)
        | _ -> 
               let st = initial.Peek().startTime
               match st with
               | y when y = moment -> 
                                      tryPush pq apq initial moment
                                      movedQueuedTasks pq apq moment
                                      let nx = getNextStamp pq initial
                                      match nx with
                                      | None -> ()
                                      | Some stamp -> travers pq apq initial stamp      
                                         
               | _ -> 
                      movedQueuedTasks pq apq moment
                      let nx = getNextStamp pq initial
                      match nx with
                      | None -> ()
                      | Some stamp -> travers pq apq initial stamp 
                      
        

    let tasks = new PQ(startTimes.Length)
    travers tasks q initial 0.0
    [|for i in 0..(startTimes.Length - 1) -> (startTimes.[0] |> float) + result.[i]|]


[<EntryPoint>]
let main _ =
    let st =  [|461620209;  461620965;  461621051;  461621056; 461621075;
    461622316;461622336;  461623323;  461623784;  461625533;
    461625785;461625852;461626762;461627149;461627246;
    461628443;461628598;461628908;461629081;461629122|]
    let be = [|6608; 9870; 7191; 8114; 6069; 4844; 848; 1865; 6274; 6776; 6114; 6142; 6588; 7426; 7921; 6123; 5964; 4144; 8753; 3998|]
    let mT = 7
    printfn "res -> %A" <| backupTimeEstimator st be mT
    0