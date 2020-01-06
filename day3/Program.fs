open System.IO

let grid =
    File.ReadAllLines(@"input.txt") |> Seq.map (fun line -> line.Split(",") |> Seq.toList)

let move (coordinate: int * int) (step: string) =
    let dir = step.[0]
    let length = int step.[1..]
    let (x, y) = coordinate

    let cxs = match dir with
        | 'U' -> List.init length (fun i -> (x, y + (i + 1)))
        | 'D' -> List.init length (fun i -> (x, y - (i + 1)))
        | 'L' -> List.init length (fun i -> (x - (i + 1), y))
        | 'R' -> List.init length (fun i -> (x + (i + 1), y))
        | _ -> failwith "unknown direction"

    List.rev cxs

let coordinates (wire: string list) =
    wire |> Seq.fold (fun cxs step -> List.append (move (Seq.head cxs) step) cxs) [ (0, 0) ]

let crossings (one: (int * int) list) (two: (int * int) list) =
    let filterTwo ((x: int), (y: int)) =
        two
        |> List.filter (fun (a, b) -> a = x && b = y)
        |> List.isEmpty
        |> not

    one |> Seq.filter filterTwo

let wires =
    Seq.map coordinates grid

let intersections =
    wires
    |> Seq.reduce (fun wire1 wire2 -> crossings wire1 wire2 |> Seq.toList)
    |> Seq.filter (fun intersection -> intersection <> (0, 0))
    
let stepsToIntersection (intersection: int * int) (wire: (int * int) list) =
    List.rev wire
    |> Seq.map (fun coordinate -> coordinate = intersection)
    |> Seq.takeWhile (not >> id)
    |> Seq.length

let delay =
    intersections
        |> Seq.map (fun intersection -> wires |> Seq.map (stepsToIntersection intersection) |> Seq.sum)
        |> Seq.min

let distance =
    intersections
    |> Seq.map (fun (x, y) -> abs x + abs y)
    |> Seq.filter (fun d -> d > 0)
    |> Seq.min

[<EntryPoint>]
let main argv =
    printfn "%i" delay
    0

