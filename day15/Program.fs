open System
open day15.Computer

type Element = | Empty | Wall | Oxygen

type Direction = | N | S | E | W

type Maze = Map<int * int, int * Element>

let dimensions (maze: Maze) =
    let cs = maze |> Map.toSeq |> Seq.map fst
    let xs, ys = cs |> Seq.map fst, cs |> Seq.map snd
    (xs |> Seq.min, xs |> Seq.max, ys |> Seq.min, ys |> Seq.max)

let printElement = function
    | Empty -> "."
    | Wall -> "#"
    | Oxygen -> "@"

let printMaze (maze: Maze) (coordinate: int * int) =
    let (minx, maxx, miny, maxy) = dimensions maze
    seq { miny..maxy } |> Seq.rev |> Seq.iter (fun y ->
        seq { minx..maxx }
        |> Seq.iter (fun x ->
            let value =
                if (x, y) = coordinate then "D"
                elif (x, y) = (0, 0) then "X"
                elif Map.containsKey (x, y) maze then printElement (snd maze.[(x, y)])
                else " "
            printf "%s" value)
        printfn "")

let add (coordinate: int * int) = function
    | N -> (fst coordinate, snd coordinate + 1)
    | S -> (fst coordinate, snd coordinate - 1)
    | W -> (fst coordinate - 1, snd coordinate)
    | E -> (fst coordinate + 1, snd coordinate)

let directionToInput = function
    | N -> 1
    | S -> 2
    | W -> 3
    | E -> 4

let turnRight = function
    | N -> E
    | S -> W
    | W -> N
    | E -> S

let toElement = function
    | 0 -> Wall
    | 1 -> Empty
    | 2 -> Oxygen
    | _ -> failwith "Invalid element"

let rec travel coordinate direction maze program n : (int option) list * Maze =
    let move coordinate : ((int option) list list) * Maze =
        let program' = run { program with Input = [ directionToInput direction |> int64 ]; Output = [] }
        let element = program'.Output |> Seq.head |> int |> toElement
        let coordinate' = add coordinate direction
        let maze' = Map.add coordinate' (n + 1, element) maze
        let reverse = (turnRight >> turnRight) direction

        match element with
            | Empty ->
                [ N; S; E; W ]
                |> List.filter (fun dir -> dir <> reverse)
                |> List.mapFold (fun maze'' dir -> travel coordinate' dir maze'' program' (n + 1)) maze'
            | Wall ->
                [ N; S; E; W ]
                |> List.filter (fun dir -> dir <> direction)
                |> List.mapFold (fun maze'' dir -> travel coordinate dir maze'' program' n) maze'
            | Oxygen ->
                ([[Some (n + 1)]], maze')

    match Map.tryFind (add coordinate direction) maze with
    | Some(d, Empty) ->
        if (n + 1) < d then move coordinate |> (fun (xs, mz) -> List.concat xs, mz)
        else ([ None ], maze)
    | Some(_, Oxygen) -> ([ Some (n + 1) ], maze)
    | Some(_, _) -> ([ None ], maze)
    | None -> move coordinate |> (fun (xs, mz) -> List.concat xs, mz)

let rec step coordinates (maze: Maze) (oxygen: Map<int * int, int>) n =
    if List.isEmpty coordinates
    then (coordinates, oxygen)
    else
        coordinates
        |> List.mapFold (fun oxygen' coordinate ->
            [N; S; E; W]
            |> List.map (add coordinate)
            |> List.filter (fun c -> not (Map.containsKey c oxygen) && snd maze.[c] = Empty)
            |> List.mapFold (fun oxygen'' coordinate' -> coordinate', Map.add coordinate (n+1) oxygen'') oxygen') oxygen
        |> (fun (cs, oxygen') -> step (List.concat cs) maze oxygen' (n+1))

[<EntryPoint>]
let main _ =
    let program = { Pointer = 0; Intcode = intcode; Input = []; Output = []; RelativeBase = 0; Halted = false }
    let maze = Map.add (0, 0) (0, Empty) Map.empty
    let (xs, maze') = travel (0, 0) N maze program 0
    let oxygenCoordinate = Map.findKey (fun _ (_, el) -> el = Oxygen) maze'
    
    printfn "Part 1"    
    printMaze maze' (0, 0)
    printfn "%A" oxygenCoordinate
    
    let steps =
        xs
        |> Seq.map (fun s ->
            match s with
            | Some v -> v
            | None -> 0)
        |> Seq.filter (fun v -> v > 0)
        |> Seq.min

    printfn "%i" steps
    
    printfn "Part 2"
    let steps =
        snd (step [oxygenCoordinate] maze' Map.empty 0)
        |> Map.toList
        |> List.maxBy (fun (c, i) -> i)
        |> snd
    printfn "Steps: %i" steps
    
    0
