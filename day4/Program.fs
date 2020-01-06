open System.IO

let range = [ 402328..864247 ]

let toPassword (value: int) =
    string value
    |> Seq.map (fun c -> int c)
    |> Seq.toList

let rec findRepeatedDigits (value: int list) =
    match value with
    | v1 :: tail -> (v1, 1 + (tail |> Seq.takeWhile (fun s -> s = v1) |> Seq.length)) :: findRepeatedDigits (List.skipWhile (fun v -> v = v1) tail)
    | [] -> []

let hasTwo (value: int list) =
    findRepeatedDigits value
    |> Seq.filter (fun (v, c) -> c = 2)
    |> Seq.isEmpty
    |> not

let rec isIncreasing (value: int list) =
    match value with
    | v1 :: v2 :: tail -> v1 <= v2 && isIncreasing (v2 :: tail)
    | _ -> true

let passwords =
    range
    |> Seq.map toPassword
    |> Seq.filter hasTwo
    |> Seq.filter isIncreasing
    |> Seq.distinct

[<EntryPoint>]
let main argv =
    printfn "%i" (Seq.length passwords)
    0

