open System.IO
open System;

type Asteroid = {
    coordinate: int * int
    sights: Map<double, (int * int) list>
 }

let data =
    File.ReadAllLines(@"input.txt")
    |> Seq.mapi (fun row line ->
        line |> Seq.mapi (fun col obj ->
            if obj = '#' then Some(col, row) else None))
    |> Seq.concat

let toVector basis coordinate =
    (double (fst coordinate - fst basis), double (snd basis - snd coordinate))

let length (x: double, y: double) = (x ** 2.0 + y ** 2.0) ** 0.5

let sight (asteroid: Asteroid) (coordinate: int * int) =
    let vector = toVector asteroid.coordinate coordinate
    let radians = atan2 (snd vector) (fst vector)
    let key = if radians < 0.0 then radians + (2.0 * Math.PI) else radians
    let sights = if Map.containsKey key asteroid.sights then asteroid.sights.[key] else []
    let sorted = List.append sights [ coordinate ] |> List.sortBy (fun c -> length (toVector asteroid.coordinate c))

    Map.add key sorted asteroid.sights

let folder (asteroid: Asteroid) (coordinate: (int * int) option) =
    let newSights =
        match coordinate with
        | None -> asteroid.sights
        | Some(i, j) -> if asteroid.coordinate = (i, j) then asteroid.sights else sight asteroid (i, j)

    { asteroid with sights = newSights }

let map =
    data |> Seq.map (fun obj ->
            match obj with
            | None -> None
            | Some(x, y) ->
                Some (data |> Seq.fold folder { coordinate = (x, y); sights = Map.empty }))

let printMap (stations: Asteroid option seq) =
    stations |> Seq.map (fun obj ->
        match obj with
        | None -> printf "."
        | Some asteroid -> printf "%i" (Map.count asteroid.sights))

let rec vaporize index radians (station: Asteroid) =
    let keys = station.sights |> Map.toSeq |> Seq.map fst |> Seq.sortDescending
    let key = Seq.find (fun k -> k <= radians) keys

    let (newIndex, newAsteroids) =
        match station.sights.[key] with
        | [] -> (index, [])
        | xs ->
            let (x, y) = List.head xs
            printfn "Vaporized %i  (%i, %i)" index x y
            (index + 1, List.tail xs)

    let newSights = Map.add key newAsteroids station.sights
    let newRadians =
        match Seq.tryFind (fun k -> k < key) keys with
        | None -> Seq.head keys
        | Some k -> k

    let isEmpty = Map.tryFindKey (fun _ xs -> not (List.isEmpty xs)) newSights |> Option.isNone

    if isEmpty then
        printfn "Done vaporizing!"
    else
        vaporize newIndex newRadians { station with sights = newSights }

[<EntryPoint>]
let main _ =
    let m = map
    let asteroid = m |> Seq.maxBy (fun obj ->
        match obj with
        | None -> 0
        | Some asteroid -> Map.count asteroid.sights)

    match asteroid with
    | None -> printfn "No asteroid found"
    | Some ast ->
        printfn "(%i,%i) = %i" (fst ast.coordinate) (snd ast.coordinate) (Map.count ast.sights)
        vaporize 0 (Math.PI / 2.0) ast
    0

