open System.IO

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

let three (a: int * int) (data: int array) input =
    Array.mapi (fun k v -> if k = fst a then input else v) data

let four (a: int * int) (data: int array) =
    resolve a data

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

let intcodeToString =
    Seq.map string >> String.concat ","

let normalizeModes modes =
        modes
        |> Seq.append (Seq.init (4 - (Seq.length modes)) (fun i -> 0))
        |> Seq.rev

type Program = {
    Pointer: int
    Intcode: int []
    Input: int list
    Output: int option
    Halted: bool
 }

let rec run (program: Program) =
    let input = program.Input
    let intcode = program.Intcode
    let pointer = program.Pointer

    let opcode = intcode.[pointer] % 100
    let modes =
       string ((intcode.[pointer] - opcode) / 100)
       |> Seq.map (fun s -> int (string s))
       |> normalizeModes
       |> Seq.toList

    match opcode with
        | 1 ->
            let newIntcode = one (intcode.[pointer + 1], modes.[0]) (intcode.[pointer + 2], modes.[1]) (intcode.[pointer + 3], modes.[2]) intcode
            run { program with Pointer = (pointer + 4); Intcode = newIntcode }

        | 2 ->
            let newIntcode = two (intcode.[pointer + 1], modes.[0]) (intcode.[pointer + 2], modes.[1]) (intcode.[pointer + 3], modes.[2]) intcode
            run { program with Pointer = (pointer + 4); Intcode = newIntcode }

        | 3 ->
            if (Seq.isEmpty input) then
                program
            else
                let newIntcode = three (intcode.[pointer + 1], modes.[0]) intcode (List.head input)
                run { program with Pointer = (pointer + 2); Intcode = newIntcode; Input = List.tail input }
        | 4 ->
            let output = four (intcode.[pointer + 1], modes.[0]) intcode
            run { program with Pointer = (pointer + 2); Output = Some output }

        | 5 ->
            let value = five (intcode.[pointer + 1], modes.[0]) (intcode.[pointer + 2], modes.[1]) intcode
            match value with
            | Some a -> run { program with Pointer = a }
            | None -> run { program with Pointer = pointer + 3 }

        | 6 -> let value = six (intcode.[pointer + 1], modes.[0]) (intcode.[pointer + 2], modes.[1]) intcode
               match value with
               | Some a -> run { program with Pointer = a }
               | None -> run { program with Pointer = pointer + 3 }

        | 7 ->
            let newIntcode = seven (intcode.[pointer + 1], modes.[0]) (intcode.[pointer + 2], modes.[1]) (intcode.[pointer + 3], modes.[2]) intcode
            run { program with Pointer = (pointer + 4); Intcode = newIntcode }

        | 8 ->
            let newIntcode = eight (intcode.[pointer + 1], modes.[0]) (intcode.[pointer + 2], modes.[1]) (intcode.[pointer + 3], modes.[2]) intcode
            run { program with Pointer = (pointer + 4); Intcode = newIntcode }

        | 99 -> { program with Halted = true }

        | _ -> failwith (intcodeToString intcode)

let rec insertions x = function
    | [] -> [ [ x ] ]
    | (y :: ys) as l -> (x :: l) :: (List.map (fun x -> y :: x) (insertions x ys))

let rec permutations = function
    | [] -> seq [ [] ]
    | x :: xs -> Seq.concat (Seq.map (insertions x) (permutations xs))

let initAmps phases =
    phases |> Seq.map (fun phase ->
        { Pointer = 0; Intcode = intcode; Input = [ phase ]; Output = None; Halted = false })

let rec feedback input amps =
    let (newAmps, output) =
        amps
        |> Seq.mapFold (fun state amplifier ->
            if amplifier.Halted then
                (amplifier, state)
            else
                let result = run { amplifier with Input = (List.append amplifier.Input [ state ]) }
                let output =
                    match result.Output with
                    | None -> failwith "No output from amplifier"
                    | Some value -> value
                (result, output)
            ) input
    let last = Seq.last newAmps
    if (last.Halted) then output else feedback output newAmps

[<EntryPoint>]
let main _ =
    let max =
        permutations [ 5; 6; 7; 8; 9 ]
        |> Seq.map initAmps
        |> Seq.map (feedback 0)
        |> Seq.max

    printfn "%i" max
    0

