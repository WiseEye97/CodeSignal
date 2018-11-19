(*
    A group of farmers has some elevation data that we are going to use to help them understand how rainfall flows over their farmland. 
    We represent the farmland as a 2D array of altitudes, the grid, and use the following model, based on the fact that water flows downhill:

        - If a cell's four neighboring cells all have altitudes not lower that its own, this cell is a sink in which water collects.
        - Otherwise, water will flow into the neighboring cell with the lowest altitude. If a cell is not a sink, you can assume it has a unique lowest neighbor and that this neighbor will be lower than the cell.
        - Cells that drain into the same sink, directly or indirectly, are part of the same basin.

    Given an n × n grid of elevations, your goal is to partition the map into basins and output the sizes of the basins, in descending order.
*)


let calculateBasins grid =
    let l1 , l2 =  Array.length grid ,  Array.length grid.[0] 
    let l = (Array.length grid) * (Array.length grid.[0])  
    let dp = [|for i in 0..(l - 1) -> i|]

    let mutable unions = dp.Length

    let getMin (i,j) =
        [(i, j - 1);(i, j + 1);(i - 1, j);(i + 1, j)]
        |> List.filter (fun (x,y) -> x >= 0 && x < l1 && y >= 0 && y < l2 )
        |> (fun li -> 
                match li with
                | [] -> -1,-1
                | _ ->  List.minBy (fun (x,y) -> grid.[x].[y]) li
            )

    let rec findRoot = function
        | x when dp.[x] = x -> x
        | x -> findRoot dp.[x]
    
    let getCell (i,j) = i*l2 + j

    for i in 0..(l1 - 1) do
        for j in 0..(l2 - 1) do
            let cell , v = getCell (i,j) , grid.[i].[j]
            let x,y = getMin(i,j)
            if x = -1 then
                ()
            else 
                let minVal = grid.[x].[y]
                let cell2 = getCell (x,y)
                let r1 , r2 = findRoot cell , findRoot cell2
                if minVal < v && r1 <> r2 then
                    dp.[cell] <- r2
                    unions <- unions - 1

    [|for i in 0..(l - 1) -> findRoot i|]
    |> Array.countBy (fun e -> e)
    |> Array.map snd
    |> Array.sortDescending