//https://app.codesignal.com/interview-practice/task/cX2iJi2STeWguq3e2

let countSmallerToTheRight (nums:int[]) =
    let rec merge i j arr1 arr2 (res : int []) cnt = 
        let l1,l2 = arr1 |> Array.length , arr2 |> Array.length
        let index = i + j
        match i,j with
        | a , b when a = l1 && b = l2 -> cnt
        | a , b when a = l1 ->               
                               res.[index] <- arr2.[b]
                               merge i (j + 1) arr1 arr2 res cnt
        | a , b when b = l2 -> 
                               res.[index] <- arr1.[a]
                               merge (i + 1) j arr1 arr2 res cnt
        | _ ->
               let v1,v2 = arr1.[i] , arr2.[j]
               match v2 - v1 with
               | x when x < 0 ->  res.[index] <- arr2.[j]
                                  let left = (int64)(arr1.Length - i)  
                                  merge i (j + 1) arr1 arr2 res (cnt + left)
               | _ -> res.[index] <- arr1.[i]
                      merge (i + 1) j arr1 arr2 res cnt

    let mutable result = 0L

    let rec mergeSort i j = 
        let l = j - i
        match l with
        | 0 -> [|nums.[j]|]
        | _ -> 
               let mid = (i + j)/2
               let t1 , t2 = mergeSort i mid, mergeSort (mid + 1) j
               let res = Array.create (t1.Length + t2.Length) 0
               let smaller = merge 0 0 t1 t2 res 0L
               result <- result + smaller
               res
               
    match nums with
    | [||] -> 0L
    | _ ->
            mergeSort 0 (nums.Length - 1) |> ignore
            result

