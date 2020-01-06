open System.IO

let input = 5

let intcode =
    File.ReadAllText(@"input.txt").Split(",") |> Seq.map int |> Seq.toArray

let resolve (op: int * int) (data: int array) =
    let value, mode = op
    match mode with
        | 0 -> data.[value]
        | 1 -> value
        | _ -> failwith ("invalid parameter mode " + (string mode))

let one (a: int * int) (b: int * int) (c: int * int) (data: int array) =
    let result = (resolve a data) + (resolve b data)
    Array.mapi (fun k v -> if k = fst c then result else v) data

let two (a: int * int) (b: int * int) (c: int * int) (data: int array) =
    let result = (resolve a data) * (resolve b data)
    Array.mapi (fun k v -> if k = fst c then result else v) data

let three (a: int * int) (data: int array) =
    Array.mapi (fun k v -> if k = fst a then input else v) data

let four (a: int * int) (data: int array) =
    printfn "%i" (resolve a data)
    data

let five (a: int * int) (b: int * int) (data: int array) =
    if (resolve a data) > 0 then Some(resolve b data) else None

let six (a: int * int) (b: int * int) (data: int array) =
    if (resolve a data) = 0 then Some(resolve b data) else None

let seven (a: int * int) (b: int * int) (c: int * int) (data: int array) =
    let value = if ((resolve a data) < (resolve b data)) then 1 else 0
    Array.mapi (fun k v -> if k = fst c then value else v) data

let eight (a: int * int) (b: int * int) (c: int * int) (data: int array) =
    let value = if ((resolve a data) = (resolve b data)) then 1 else 0
    Array.mapi (fun k v -> if k = fst c then value else v) data

let formatIntcode =
    Seq.map string >> String.concat ","

let normalizeModes modes =
        modes
        |> Seq.append (Seq.init (4 - (Seq.length modes)) (fun i -> 0))
        |> Seq.rev

let rec run i (l: int []) =
    let opcode = l.[i] % 100
    let modes =
       string ((l.[i] - opcode) / 100)
       |> Seq.map (fun s -> int (string s))
       |> normalizeModes
       |> Seq.toList

    match opcode with
        | 1 -> run (i + 4) (one (l.[i + 1], modes.[0]) (l.[i + 2], modes.[1]) (l.[i + 3], modes.[2]) l)
        | 2 -> run (i + 4) (two (l.[i + 1], modes.[0]) (l.[i + 2], modes.[1]) (l.[i + 3], modes.[2]) l)
        | 3 -> run (i + 2) (three (l.[i + 1], modes.[0]) l)
        | 4 -> run (i + 2) (four (l.[i + 1], modes.[0]) l)
        | 5 -> let result = five (l.[i + 1], modes.[0]) (l.[i + 2], modes.[1]) l
               match result with
               | Some a -> run a l
               | None -> run (i + 3) l
        | 6 -> let result = six (l.[i + 1], modes.[0]) (l.[i + 2], modes.[1]) l
               match result with
               | Some a -> run a l
               | None -> run (i + 3) l
        | 7 -> run (i + 4) (seven (l.[i + 1], modes.[0]) (l.[i + 2], modes.[1]) (l.[i + 3], modes.[2]) l)
        | 8 -> run (i + 4) (eight (l.[i + 1], modes.[0]) (l.[i + 2], modes.[1]) (l.[i + 3], modes.[2]) l)
        | 99 -> l
        | _ -> failwith (formatIntcode l)


[<EntryPoint>]
let main argv =
    formatIntcode (run 0 intcode) |> printfn "%s"
    0

