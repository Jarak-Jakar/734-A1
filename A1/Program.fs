// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open Akka
open Akka.FSharp

let findLCSLenSeq (topString: array<char>) (sideString: array<char>) = 
    let upperBoundArray = Array.zeroCreate(topString.Length)
    let sideBoundArray = Array.zeroCreate(sideString.Length)
    let vectorOne = Array.copy(upperBoundArray)
    vectorOne.GetValue(0)

let checkCell firstChar secondChar top left topLeft = 
    if firstChar = secondChar then topLeft + 1
    else max top left

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    printfn "Hello world!"
    printfn "%A" argv.[2]
    //0 // return an integer exit code
    let stringOne = System.IO.File.ReadAllText(argv.[0].[4..]).ToCharArray()
    let stringTwo = System.IO.File.ReadAllText(argv.[1].[4..]).ToCharArray()
    //printfn "%A" stringOne
    //printfn "%A" stringTwo
    //printfn "%A" (findLCSLenSeq stringOne stringTwo 3 4)
    let LCSLen = if stringOne.Length > stringTwo.Length then findLCSLenSeq stringTwo stringOne
                 else findLCSLenSeq stringOne stringTwo
    printfn "%A" LCSLen
    0 // return an integer exit code