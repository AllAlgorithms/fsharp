(*
Merge Sort implemented in F#
Author: Carlos Abraham
Source: https://github.com/AllAlgorithms/fsharp
Run it: https://repl.it/@abranhe/MergeSort-in-F
*)
let rec mergeSort list =
    let rec merge firstList secondList acc =
        match firstList, secondList with
        | [], [] -> List.rev acc
        | head::tail, [] | [], head::tail -> merge tail [] (head :: acc)
        | fHead::fTail, sHead::sTail when fHead < sHead -> merge fTail  secondList (fHead :: acc)
        | fHead::fTail, sHead::sTail -> merge firstList  sTail (sHead :: acc)

    match list with
    | [] -> list
    | [a] -> list
    | _ ->
        let first, second = List.splitAt(List.length list / 2) list
        merge (mergeSort first) (mergeSort second) []

[<EntryPoint>]
printfn "%A" (mergeSort [3; 7; 19; 15])
printfn "%A" (mergeSort [-22; 4; 1; 77; 27; 3; -134])
let bigList = List.init 10000 (fun i -> -i)
printfn "%A" <| ((mergeSort bigList) = (List.rev bigList))
