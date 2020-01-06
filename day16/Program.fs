open System.IO

let input = File.ReadAllText("input.txt") |> Seq.map (string >> int)

module Seq = let rec cycle xs = seq { yield! xs; yield! cycle xs }

let basePattern = [0; 1; 0; -1]

let pattern n =
    basePattern
    |> Seq.map (Seq.replicate n)
    |> Seq.concat
    |> Seq.cycle
    |> Seq.tail

let dotProduct xs ys =
    Seq.map2 (*) xs ys |> Seq.sum

let onesDigit x = abs x % 10

let fft1 xs =
    seq { 1 .. (Seq.length xs) }
    |> Seq.toList
    |> List.map (fun i ->
        pattern i
        |> dotProduct xs
        |> onesDigit)

let rec fft phaser n xs =
    match n with
    | 0 -> xs
    | n -> fft phaser (n - 1) (phaser xs)    

let fftend1 input =
    List.scanBack (+) input 0 |> List.map onesDigit

let signal =
    Seq.replicate 10000 input |> Seq.concat
    
[<EntryPoint>]
let main _ =
    printfn "Part 1"
    let result = input |> Seq.toList |> fft fft1 100
    printfn "%A" result

    printfn "Part 2"
    printfn "%A" signal
    
    printfn "Length: %i" (Seq.length signal)
    let signal' = signal |> Seq.toList
    let offset =  signal'.[0..6] |> List.fold (fun s y -> s + string y) "" |> int
    printfn "Offset: %i" offset

    let signal' = List.splitAt offset signal' |> snd        
    let output = fft fftend1 100 signal'
    let message = output.[0 .. 7]

    printfn "%A" message
    
    0


