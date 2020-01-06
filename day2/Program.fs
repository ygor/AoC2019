open System.IO

let intcode =
    File.ReadAllText(@"input.txt").Split(",") |> Seq.map int |> Seq.toArray

let one a b c (st: int array) =
    let result = st.[a] + st.[b]
    Array.mapi (fun k v -> if k = c then result else v) st

let two a b c (st: int array) =
    let result = st.[a] * st.[b]
    Array.mapi (fun k v -> if k = c then result else v) st

let formatIntcode =
    Seq.map string >> String.concat ","

let rec run i (l: int []) =
    let opcode = l.[i]

    match opcode with
        | 1 -> run (i + 4) (one l.[i + 1] l.[i + 2] l.[i + 3] l)
        | 2 -> run (i + 4) (two l.[i + 1] l.[i + 2] l.[i + 3] l)
        | 99 -> l
        | _ -> failwith (formatIntcode l)

let replace1202 noun verb (l: int []) =
    Seq.mapi (fun index v -> if index = 1 then noun else if index = 2 then verb else v) l
    |> Seq.toArray

let findIntcode =
    let target = 19690720
    let ps = seq { 0..99 }

    let intcodes = ps |> Seq.map (fun i -> (ps |> Seq.map (fun j -> (i, j, run 0 (replace1202 i j intcode))))) |> Seq.concat
    let _, _, code = Seq.filter (fun (_, _, (codes: int [])) -> codes.[0] = target) intcodes |> Seq.head
    code

[<EntryPoint>]
let main argv =
    formatIntcode findIntcode |> printfn "%s"
    0

