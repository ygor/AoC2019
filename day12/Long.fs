module day12.Long

let input = [
    [ 5; 4; 4 ];
    [ -11; -11; -3 ];
    [ 0; 7; 0 ];
    [ -13; 2; 10 ] ]

let rec gcd (x: int64) (y: int64) = if y = int64 0 then abs x else gcd y (x % y)

let lcm x y = x * y / (gcd x y)

type Moon = int list * int list

let delta x y = if x < y then 1 elif x > y then -1 else 0

let accelerate (moons: Moon list) =
    moons |> List.map (fun m1 ->
        let newVelocity =
            moons
            |> List.map (fun m2 -> List.map2 delta (fst m1) (fst m2))
            |> List.reduce (List.map2 (+))
            |> List.map2 (+) (snd m1)
        (fst m1, newVelocity))

let move (a: Moon) = List.map2 (+) (fst a) (snd a), snd a

let tick (moons: Moon list) = moons |> accelerate |> List.map move

let energy (vector: int list) = vector |> List.map abs |> List.sum

let totalEnergy (moon: Moon) = (fst moon |> energy) * (snd moon |> energy)

let hashMoon axis (moon: Moon) =
    sprintf "%i.%i" (fst moon).[axis] (snd moon).[axis]

let hashMoons axis (moons: Moon list) = moons |> Seq.map (hashMoon axis) |> Seq.reduce (+)

let firstRepeatAxis (moons: Moon list) axis =
    let state = hashMoons axis moons
    moons
    |> Seq.unfold (fun moons -> tick moons |> (fun ms -> Some(ms, ms)))
    |> Seq.takeWhile (fun moons -> hashMoons axis moons <> state)
    |> Seq.length
    |> (+) 1

let firstRepeat (moons: Moon list) =
    [ 0; 1; 2 ]
    |> Seq.map (int64 << firstRepeatAxis moons)
    |> Seq.reduce lcm

let printMoon (moon: Moon) = printfn "[%i, %i, %i]" (fst moon).[0] (fst moon).[1] (fst moon).[2]

let rec runTicks n (moons: Moon list) =
    match n with
    | 0 -> moons
    | _ -> tick moons |> runTicks (n - 1)

//[<EntryPoint>]
let main _ =
    let moons = input |> List.map (fun xs -> (xs, [ 0; 0; 0 ]))
    printfn "alignment after %i steps" (firstRepeat moons)

    let constellation = moons |> runTicks 100

    constellation |> List.iter printMoon
    constellation |> List.map totalEnergy |> List.sum |> printfn "Total energy: %i"

    0

