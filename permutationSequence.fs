open System.Text

(*
    The set [1, 2, 3, ... , n] contains a total of n! unique permutations. 
    List all the permutations for an integer n in lexicographical order and return the kth permutation in the sequence as a string. 
    To build this string, concatenate decimal representations of permutation elements from left to right without any delimiters.
*)

let permutationSequence n k =

    let rec toFactorial = function
        | 0 , li -> li
        | x , li -> 
                    let d = (li |> List.length) + 1
                    let r = x%d
                    toFactorial ((x/d),r::li)
    let rec generateString li (res : StringBuilder) (strLi : string list)  = 
        match li with
        | [] -> res
        | h::t -> 
                  strLi 
                  |> List.splitAt h
                  |> (function 
                    | l1 , l2 -> 
                                res.Append(List.head l2) |> ignore
                                generateString t res (l1 @ (List.tail l2))
                  )
    let fNum = toFactorial(k - 1,[])
    let s =  [for i in 1..n -> i |> string ]
    let missing = s.Length - fNum.Length
    generateString (List.concat [[for x in 1..missing -> 0] ; fNum]) (new StringBuilder()) s
    |> (fun sb -> sb.ToString())   