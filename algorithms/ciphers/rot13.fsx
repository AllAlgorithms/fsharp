(* Rot13 implemented in F# *)

open System.Text

let rotChar (c:char) offset = int c + offset |> char

let rot13 (str:string) =
    let rec rot (str:char list) (sb:StringBuilder) =
        match str with
        | [] -> sb.ToString()
        | c::cs -> match int c with
                   | cCode when (cCode > 77 && cCode <= 90) || (cCode > 109 && cCode <= 122) -> rot cs (rotChar c -13 |> sb.Append)
                   | _ -> rot cs (rotChar c 13 |> sb.Append)
        
    rot (str.ToCharArray() |> List.ofArray) (StringBuilder())


(* Testing Rot13 *)

printfn "%s" (rot13 "hello")