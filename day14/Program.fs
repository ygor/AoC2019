open System.IO

type Chemical = int64 * string

type Reaction = Chemical array * Chemical

let updateMap key value updater map =
    if Map.containsKey key map then
        Map.add key (updater value map.[key]) map
    else
        Map.add key value map

let parseChemical (raw: string): Chemical =
    let parts = raw.Trim().Split(" ")
    (int64 parts.[0], parts.[1])

let parseReaction (line: string): Reaction =
    let sides = line.Split("=>")
    (sides.[0].Split(",") |> Array.map parseChemical, parseChemical sides.[1])

let reactions = File.ReadAllLines(@"input.txt") |> Array.map parseReaction

let rec reduceWaste (chemical:Chemical) (waste: Map<string, int64>) =
    let (q, key) = chemical
    if Map.containsKey key waste then
        let w = waste.[key]
        let (q', w') = if q > w then (q - w, int64 0) else (int64 0, w - q)
        ((q', key), Map.add key w' waste)  
    else
        (chemical, waste)

let rec reduceFuel (chemical: Chemical) (waste: Map<string, int64>) (reactions: Reaction array) =
    let runReaction (reaction:Reaction) (waste: Map<string, int64>) =
        let (c, waste') = reduceWaste chemical waste
        let q = snd reaction |> fst
        let times = int64 (ceil (float (fst c) / float q))
        let w = times * q - (fst c) 
        let waste'' = updateMap (snd chemical) w (+) waste'
        
        fst reaction |> Seq.fold (fun (inputs, ws) inp ->
            let (is, ws') = reduceFuel (fst inp * times, snd inp) ws reactions
            (List.append is inputs, ws')) ([], waste'')
            
    let reaction = reactions |> Seq.tryFind (fun reaction -> snd (snd reaction) = snd chemical)
    match reaction with
    | Some r -> runReaction r waste
    | None -> ([ chemical ], waste)

let rec maxFuel n =
    let (inputs, waste) = reduceFuel (n, "FUEL") Map.empty reactions
    let quantity = inputs |> List.map fst |> List.sum
    if quantity > int64 1000000000000.0 then
        n
    else
        maxFuel (n + int64 1)

[<EntryPoint>]
let main _ =
    let (inputs, waste) = reduceFuel (int64 1, "FUEL") Map.empty reactions
    let quantity = inputs |> List.map fst |> List.sum
    printfn "%i" quantity

    let max = maxFuel (int64 3343470)
    printfn "%i" max 
    0

