(*
Fibonacci implemented in F#
Author: Carlos Abraham
Source: https://github.com/AllAlgorithms/fsharp
Run it: https://repl.it/@abranhe/fibonacci-in-F
*)
let rec fibonacci n =
    match n with
    | 0 | 1 -> 1
    | _ -> fibonacci(n - 1) + fibonacci(n - 2)

// Test
let rec output n =
    if n >= 0 then
        printfn "%i - %i" n (fibonacci n)
        output(n - 1)

[<EntryPoint>]
output 40
