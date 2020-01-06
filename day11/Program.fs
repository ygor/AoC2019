open day11.Computer

let turn (side: int) (dir: int * int) =
    match side with
    | 0 -> (-1 * snd dir, fst dir)
    | 1 -> (snd dir, -1 * fst dir)
    | _ -> failwith (sprintf "Invalid side %i" side)
 
let rec step (dir: int * int) (coordinate: int * int) program (grid: Map<int * int, int64 * int>) =
    let panel = if Map.containsKey coordinate grid then grid.[coordinate] else (int64 0, 0)

    let newProgram = run { program with Input = [ fst panel ]; Output = [] }
    if (newProgram.Halted) then
        grid
    else
        let output = newProgram.Output
        let newGrid = Map.add coordinate (output.[0], snd panel + 1) grid
        let newDir = turn (int output.[1]) dir
        let newCoord = (fst coordinate + fst newDir, snd coordinate + snd newDir)
        step newDir newCoord newProgram newGrid

let printGrid (grid: Map<int * int, int64 * int>) =
    let keys = Map.toList grid |> List.map fst
    let rangeX = seq { List.minBy fst keys |> fst .. List.maxBy fst keys |> fst } 
    let rangeY = seq { List.minBy snd keys |> snd .. List.maxBy snd keys |> snd } |> Seq.rev
    
    rangeY |> Seq.iter (fun y ->
        rangeX |> Seq.iter (fun x ->
            let coordinate = (x, y)
            if Map.containsKey coordinate grid then
                let value = fst grid.[coordinate]
                printf (if value = int64 0 then "." else "#") 
            else
                printf ".")
        printf "\n")
    
    
[<EntryPoint>]
let main _ =
    let program = { Pointer = 0; Intcode = intcode; Input = []; Output = []; RelativeBase = 0; Halted = false }
    let grid =
        Map.empty
        |> Map.add (0,0) (int64 1, 0)
        |> step (0, 1) (0, 0) program 
    
    grid
    |> Map.filter (fun _ (_, visits) -> visits > 0)
    |> Map.count
    |> printfn "%i"
    
    printGrid grid
    0

