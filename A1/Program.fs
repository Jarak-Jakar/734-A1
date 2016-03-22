// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open Akka
open Akka.FSharp

let findLCSLenSeq (topString: array<char>) (sideString: array<char>) (upperBoundaryList: array<int>) (leftBoundaryList: array<int>) = 
    let vectorOne = Array.copy(upperBoundaryList)
    let vectorTwo = Array.copy(vectorOne)
    let vectorThree = Array.copy(vectorTwo)
    vectorOne.GetValue(0)

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
    let LCSLen = if stringOne.Length > stringTwo.Length then (findLCSLenSeq stringTwo stringOne (Array.zeroCreate stringTwo.Length) (Array.zeroCreate stringOne.Length))
                 else findLCSLenSeq stringOne stringTwo (Array.zeroCreate stringOne.Length) (Array.zeroCreate stringTwo.Length)
    printfn "%A" LCSLen
    0 // return an integer exit code