open System
open day15.Computer

type Direction = | N | S | W | E

type Element = | Space | Scaffold | Robot of Direction

module Element =
    let parse = function
        | '#' -> Scaffold
        | '.' -> Space
        | '>' -> Robot E
        | '<' -> Robot W
        | '^' -> Robot N
        | 'v' -> Robot S
        | x -> failwith (sprintf "Invalid element %c" x)

    let toChar = function
        | Scaffold -> '#'
        | Space -> '.'
        | Robot E -> '>'
        | Robot W -> '<'
        | Robot N -> '^'
        | Robot S -> 'v'
        | _ -> failwith "Invalid element"

type View = Map<int * int, Element>

module View =
    let dimensions (view: View) =
        let keys = view |> Map.toList |> List.map fst
        (keys |> List.maxBy fst |> fst, keys |> List.maxBy snd |> snd)
        
    let parse output: View =
        output
        |> List.map (int >> Char.ConvertFromUtf32)
        |> List.reduce (+)
        |> fun s -> s.Split "\n"
        |> Array.mapi (fun y line ->
            line.ToCharArray() |> Array.mapi (fun x char -> (x,y), Element.parse char))
        |> Array.concat
        |> Map.ofArray

    let print (view: View) =
        let (maxx, maxy) = dimensions view
        seq {0 .. maxy } |> Seq.iter (fun y ->
            seq { 0 .. maxx } |> Seq.iter (fun x ->
                (Element.toChar >> printf "%c") view.[(x,y)] ) 
            printfn "")

let add coordinate = function
    | N -> (fst coordinate, snd coordinate + 1 )
    | S -> (fst coordinate, snd coordinate - 1 )
    | W -> (fst coordinate - 1, snd coordinate )
    | E -> (fst coordinate + 1, snd coordinate )

let isIntersection coordinate (view:View) =
    [N;S;W;E]
    |> List.map (fun dir ->
        let coordinate' = add coordinate dir
        if Map.containsKey coordinate' view then view.[coordinate'] = Scaffold else false)
    |> List.reduce (&&)
    |> (&&) (view.[coordinate] = Scaffold)
            
let intersections (view:View) =
    view
    |> Map.filter (fun coordinate _ -> isIntersection coordinate view)
    |> Map.toList
    |> List.map fst

let start (view: View) =
    view |> Map.findKey (fun key el ->
        match el with
        | Robot _ -> true
        | _ -> false )

let rec walk coordinate (view: View) =
    [N;S;E;W] |> List.map (fun dir ->
        )

let paths (view: View) =
    let coordinate = start view
    

[<EntryPoint>]
let main _ =
    let program = run { Pointer = 0; Intcode = intcode; Input = []; Output = []; RelativeBase = 0; Halted = false }
    let view = program.Output |> View.parse
    View.print view

    let intersections = intersections view
    printfn "%A" intersections
    
    let alignment = intersections |> List.map (fun (x,y) -> x * y) |> List.sum
    printfn "Alignment: %i" alignment
    0