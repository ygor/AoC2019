open System.IO

let data =
    File.ReadAllLines(@"input.txt")
    |> Seq.map (fun s -> s.Split(")"))
    |> Seq.toList

type Tree =
    | Leaf of string
    | Branch of string * Tree seq

let children id = 
    data |> List.filter (fun orbit -> orbit.[0] = id)

let rootId = "COM"
let youId = "YOU"
let santaId = "SAN"

let rec getTree (id:string) (data: (string []) list) =
    let ids = children id |> List.map (fun child -> getTree child.[1] data)
    match ids with
    | [] -> Leaf id
    | xs -> Branch (id, xs)

let rec toString (tree:Tree) =
    match tree with
    | Leaf s -> s
    | Branch (s, nodes) ->  Seq.map (fun node -> toString node) nodes |> String.concat " | " |> sprintf "(%s -> %s)" s  

let rec orbits (depth:int) (tree:Tree) =
    match tree with
    | Leaf _ -> depth
    | Branch (_, nodes) -> depth + (nodes |> Seq.map (orbits (depth + 1)) |> Seq.sum)

let rec maxDepth (tree: Tree) =
    match tree with
    | Leaf s -> 0
    | Branch (s, nodes) -> 1 + (if (Seq.isEmpty nodes) then 0 else nodes |> Seq.map maxDepth |> Seq.max)

let rec filterLeaves predicate (tree:Tree) =
    match tree with
    | Leaf s -> if (predicate s) then Some (Leaf s) else None
    | Branch (s, nodes) ->
        let subtrees =
            nodes
            |> Seq.map (filterLeaves predicate)
            |> Seq.filter Option.isSome
            |> Seq.map Option.get
        
        if (Seq.isEmpty subtrees) then None else Some (Branch (s, subtrees))

let rec filterCommonBranches (tree1:Tree) (tree2: Tree) =
    match tree1 with
    | Leaf s -> None
    | Branch (s, nodes) ->
        match tree2 with
        | Leaf s2 -> None
        | Branch (s2, nodes2) ->
            if (s = s2) then
                let subtrees =
                    nodes
                    |> Seq.map (fun node -> nodes2 |> Seq.map (fun node2 -> filterCommonBranches node node2))
                    |> Seq.concat
                    |> Seq.filter Option.isSome
                    |> Seq.map Option.get
                Some (Branch (s, subtrees))                                
            else
                None

[<EntryPoint>]
let main argv =
    let tree = getTree rootId data
    let youOption = filterLeaves (fun s -> s = youId) tree
    let santaOption = filterLeaves (fun s -> s = santaId) tree
    
    match youOption with
    | None -> printfn "%s" "YOU not found"
    | Some you ->
        printfn "%s" (toString you)
        let youDepth = maxDepth you
        printfn "%i" (youDepth)
        
        match santaOption with
        | None -> printfn "%s" "SAN not found"
        | Some santa ->
            printfn "%s" (toString santa)

            let santaDepth = maxDepth santa
            printfn "%i" (santaDepth)

            let commonOption = filterCommonBranches you santa
            match commonOption with
            | None -> printfn "%s" "Common not found"
            | Some common ->
                printfn "%s" (toString common)

                let commonDepth = maxDepth common
                printfn "%i" (commonDepth)
                printfn "%i" (youDepth - commonDepth + santaDepth - commonDepth)                

    0
