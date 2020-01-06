open System.IO

type Program = {
    Pointer: int
    Intcode: int64 []
    Input: int64 list
    Output: int64 list
    RelativeBase: int
    Halted: bool
 }

let sourceIntCode =
    File.ReadAllText(@"input.txt").Split(",") |> Seq.map int64 |> Seq.toArray

let normalizeModes modes =
    modes
    |> Seq.append (Seq.init (4 - (Seq.length modes)) (fun i -> 0))
    |> Seq.rev

let modes (program: Program) =
    let value = string program.Intcode.[program.Pointer]

    value.[0..(String.length value - 3)]
    |> Seq.map (fun s -> int (string s))
    |> normalizeModes
    |> Seq.toArray

let read (address: int64 * int) (program: Program) =
    let value, mode = address
    let get index =
        if index < int64 (Array.length program.Intcode) then
            program.Intcode.[int index]
        else int64 0

    match mode with
        | 0 -> get value // position
        | 1 -> int64 value // immediate
        | 2 -> get (int64 program.RelativeBase + value) // relative
        | _ -> failwith (sprintf "invalid parameter mode: %i" mode)

let write (address: int64 * int) (value: int64) (program: Program) =
    let relativePointer, mode = address
    let pointer = match mode with
    | 2 -> program.RelativeBase + (int relativePointer)
    | _ -> int relativePointer

    let increase = pointer - (Array.length program.Intcode) + 1
    let updateMemory = Array.mapi (fun k v -> if k = pointer then value else v)

    if increase <= 0 then
        updateMemory program.Intcode
    else
        Array.append program.Intcode (Array.zeroCreate increase) |> updateMemory

let oneTwo (program: Program) operation =
    let { Intcode = intcode; Pointer = pointer } = program
    let args = Array.zip intcode.[(pointer + 1)..(pointer + 3)] (modes program).[0..2]
    let value = operation (read args.[0] program) (read args.[1] program)
    let newIntcode = write args.[2] value program

    { program with Pointer = (pointer + 4); Intcode = newIntcode }
    
let one (program: Program) = oneTwo program (+)
let two (program: Program) = oneTwo program (*)    

let three (program: Program) =
    let { Intcode = intcode; Pointer = pointer } = program
    let arg = (intcode.[pointer + 1], (modes program).[0])
    let value = List.head program.Input
    let newIntcode = write arg value program

    { program with Pointer = (pointer + 2); Intcode = newIntcode; Input = List.tail program.Input }

let four (program: Program) =
    let { Intcode = intcode; Pointer = pointer; Output = output } = program
    let arg = (intcode.[pointer + 1], (modes program).[0])
    let value = read arg program

    { program with Pointer = (pointer + 2); Output = List.append output [ value ] }

let fiveSix (program:Program) operation =
    let { Intcode = intcode; Pointer = pointer } = program
    let args = Array.zip intcode.[(pointer + 1)..(pointer + 2)] (modes program).[0..1]    
    let value = if operation (read args.[0] program) (int64 0) then Some(read args.[1] program) else None
    
    match value with
    | Some a -> { program with Pointer = int a }
    | None -> { program with Pointer = pointer + 3 }
    
let five (program: Program) = fiveSix program (>)
let six (program: Program) = fiveSix program (=)

let sevenEight (program:Program) operation =  
    let { Intcode = intcode; Pointer = pointer } = program
    let args = Array.zip intcode.[(pointer + 1)..(pointer + 3)] (modes program).[0..2]
    let value = if operation (read args.[0] program) (read args.[1] program) then int64 1 else int64 0
    let newIntcode = write (args.[2]) value program

    { program with Pointer = (pointer + 4); Intcode = newIntcode }

let seven (program: Program) = sevenEight program (<)
let eight (program: Program) = sevenEight program (=)

let nine (program: Program) =
    let { Intcode = intcode; Pointer = pointer; RelativeBase = relativeBase } = program
    let arg = (intcode.[pointer + 1], (modes program).[0])
    let newRelativeBase = int64 relativeBase + read arg program

    { program with Pointer = (pointer + 2); RelativeBase = int newRelativeBase }

let intcodeToString (_: int64 []) =
    Seq.map string >> String.concat ","

let opcode (program: Program) =
    (int program.Intcode.[program.Pointer]) % 100

let rec run (program: Program) =
    let opcode = opcode program

    match opcode with
        | 1 ->
            run (one program)
        | 2 ->
            run (two program)
        | 3 ->
            if (Seq.isEmpty program.Input) then
                program
            else
                run (three program)
        | 4 ->
            run (four program)
        | 5 ->
            run (five program)
        | 6 ->
            run (six program)
        | 7 ->
            run (seven program)
        | 8 ->
            run (eight program)
        | 9 ->
            run (nine program)
        | 99 ->
            { program with Halted = true }
        | _ ->
            failwith (sprintf "Unknown upcode: %i" opcode)

[<EntryPoint>]
let main _ =
    let result = run { Pointer = 0; Intcode = sourceIntCode; Input = [ int64 2 ]; Output = []; RelativeBase = 0; Halted = false }
    printfn "%s" (String.concat "," (Seq.map string result.Output))

    0

