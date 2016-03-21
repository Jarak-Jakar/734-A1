// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open Akka
open Akka.FSharp

let findLCSLenSeq (inputStringOne: List<char>) (inputStringTwo: List<char>) upperBoundaryList leftBoundaryList = 
    inputStringOne.Length

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    printfn "Hello world!"
    printfn "%A" argv.[2]
    //0 // return an integer exit code
    let stringOne = System.IO.File.ReadAllText(argv.[0].[4..]).ToCharArray() |> Array.toList
    let stringTwo = System.IO.File.ReadAllText(argv.[1].[4..]).ToCharArray() |> Array.toList
    //printfn "%A" stringOne
    //printfn "%A" stringTwo
    printfn "%A" (findLCSLenSeq stringOne stringTwo 3 4)
    0 // return an integer exit code