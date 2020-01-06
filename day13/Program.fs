open day13.Computer

type Element = Empty | Wall | Block | Paddle | Ball | Score of int

type Game = Map<int*int, Element>

let toGame (output: (int []) seq) =
    let codeToElement = function
    | 0 -> Empty
    | 1 -> Wall
    | 2 -> Block
    | 3 -> Paddle
    | 4 -> Ball
    | x -> Score x
    
    output
    |> Seq.map (fun xs -> ((xs.[0], xs.[1]), codeToElement xs.[2]))
    |> Map.ofSeq

let dimensions (game: Game) =
    let xs = game |> Map.toSeq |> Seq.map fst
    (xs |> Seq.map fst |> Seq.max, xs |> Seq.map snd |> Seq.max)
 
let printElement = function
    | Empty -> " "
    | Wall -> "*"
    | Block -> "#"
    | Paddle -> "-"
    | Ball -> "0"
    | _ -> failwith "Invalid element"

let printScore = function
    | Score x -> printfn "%i" x
    | _ -> printfn "Invalid termination"
    
let printGame game =
    let (maxx, maxy) = dimensions game
    seq {0 .. maxy} |> Seq.iter (fun y ->
        seq { 0 .. maxx }
        |> Seq.iter (fun x -> printf "%s" (printElement game.[(x, y)]))
        printfn "")
    printScore game.[(-1,0)]

let rec step (program: Program) =
    let result = run program
    let game = result |> (fun r -> r.Output) |> Seq.map int |> Seq.chunkBySize 3 |> toGame
    let ball = game |> Map.pick (fun k e -> if e = Ball then Some k else None)
    let paddle = game |> Map.pick (fun k e -> if e = Paddle then Some k else None)
    
    if result.Halted then
        game
    else
        let bx, px = fst ball, fst paddle
        printGame game
        step { result with Input = [ (if bx < px then int64 -1 elif bx > px then int64 1 else int64 0) ] }

[<EntryPoint>]
let main _ =
    let freeIntcode = Array.append ([int64 2] |> List.toArray) (Array.tail intcode)
    let program = { Pointer = 0; Intcode = freeIntcode; Input = []; Output = []; RelativeBase = 0; Halted = false }
    let game = step program
    
    printGame game 
    
    0

