open System.IO

let input =
    File.ReadAllText(@"input.txt") |> Seq.map (string >> int64)

let width = 25
let height = 6
let layerSize = width * height

let layers = Seq.chunkBySize layerSize input

let maxZeroLayer =
    layers
    |> Seq.map (fun layer -> (layer, layer |> Seq.filter (fun i -> i = int64(0)) |> Seq.length))
    |> Seq.minBy (fun (_, count) -> count)
    |> fst

let signature =
    let numOnes = Seq.filter (fun i -> i = int64(1)) maxZeroLayer |> Seq.length
    let numTwos = Seq.filter (fun i -> i = int64(2)) maxZeroLayer |> Seq.length
    numOnes * numTwos

let image =
    seq { 0 .. (layerSize-1) } |> Seq.map (fun i ->
        layers
        |> Seq.map (fun l -> l.[i])
        |> Seq.reduce (fun acc x -> if acc = int64(2) then x else acc )
        )

[<EntryPoint>]
let main _ =
    image
    |> Seq.chunkBySize width
    |> Seq.iter (fun row -> printfn "%s" (String.concat "" (Seq.map string row)))
    
    0
