type Pair = (int list) * (int list) 

let step1 ((xs, vxs): Pair) =
    let diff x y = if x < y then 1 elif x > y then -1 else 0
    let diffs ys x = List.map (diff x) ys |> List.sum
    let vxs' = xs |> List.map (diffs xs) |> List.map2 (+) vxs
    (List.map2 (+) xs vxs', vxs')

let rec step (pairs: Pair list) = function
    | 0 -> pairs
    | n -> step (List.map step1 pairs) (n - 1)

let energy (pairs: Pair list) =
    let e proj = List.map (proj >> (List.map abs) >> List.sum) pairs
    List.map2 (*) (e fst) (e snd) |> List.sum

let rec findRepeat i map pair =
    if Map.containsKey pair map then i - map.[pair]
    else findRepeat (i+1) (Map.add pair i map) (step1 pair)

let rec gcd x y  = if y = int64 0 then abs x else gcd y (x % y)

let lcm x y = x * y / (gcd x y)

[<EntryPoint>]
let main _ =
    let xs = [[5; -11; 0; -13]; [4;-11;7;2]; [4;-3;0;10]] // list of x, y and z coordindates of 4 moons
    let pairs = List.map (fun xs -> (xs, Array.zeroCreate (List.length xs) |> Array.toList)) xs

    printfn "Part 1"
    printfn "Energy %i" (energy (step pairs 100))
    
    printfn "Part 2"
    printfn "Period %i" (List.map (findRepeat 1 Map.empty >> int64) pairs |> List.reduce lcm)  
    0
