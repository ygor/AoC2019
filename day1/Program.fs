open System
open System.IO

let modules =
    File.ReadAllLines(@"input.txt") |> Seq.map float

let fuel mass =
    Math.Max(Math.Floor(mass / 3.0) - 2.0, 0.0)

let rec deepFuel totalFuel =
    if totalFuel > 0.0 then
        let massFuel = fuel totalFuel
        totalFuel + (deepFuel massFuel)
    else
        totalFuel

let totalFuel =
    Seq.map fuel modules
    |> Seq.map deepFuel
    |> Seq.sum

[<EntryPoint>]
let main argv =
    totalFuel |> printf "%f"
    0

