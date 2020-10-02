(* Quick Sort implemented in F# *)

let rec quicksort = function
    | [] -> []
    | pivot :: rest ->
        let left, right = rest |> List.partition (fun x -> x < pivot)
        quicksort left @ [pivot] @ quicksort right

(* Testing sort function *)
printfn "%A" (quicksort [8; 3; 4; 1; 2])
printfn "%A" (quicksort [1.3; 0.2; -0.1; 91.4; 31.2])
printfn "%A" (quicksort ["function"; "category"; "types"; "hello world"])