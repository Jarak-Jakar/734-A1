// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open Akka
open Akka.FSharp


let checkCell firstChar secondChar top left topLeft = 
    if firstChar = secondChar then (firstChar, secondChar, (topLeft + 1))
    else (firstChar, secondChar, (max top left))

let findLCSLenSeq (topString: array<char>) (sideString: array<char>) (topValues: array<int>) (sideValues: array<int>) = 
    let vectorOne = Array.zeroCreate(topString.Length)
    let vectorTwo = Array.zeroCreate(topString.Length)
    let vectorThree = Array.zeroCreate(topString.Length)

    //Initialise vector values
    //vectorOne.[0] <- (topIndex, sideIndex, topValues)
    vectorOne.[0] <- (0, 0, topValues.[1])
    vectorTwo.[0] <- (0, -1, 0)
    vectorTwo.[1] <- (-1, 0, 0)
    vectorThree.[0] <- (-1, -1, 0)

    //First step
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
    let LCSLen = if stringOne.Length > stringTwo.Length then findLCSLenSeq stringTwo stringOne (Array.zeroCreate(stringTwo.Length + 1)) (Array.zeroCreate(stringOne.Length + 1))
                 else findLCSLenSeq stringOne stringTwo (Array.zeroCreate(stringOne.Length + 1)) (Array.zeroCreate(stringTwo.Length + 1))
    printfn "%A" LCSLen
    0 // return an integer exit code