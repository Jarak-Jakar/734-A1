// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open Akka
open Akka.FSharp


let checkCell firstChar secondChar top left topLeft = 
    if firstChar = secondChar then (firstChar, secondChar, (topLeft + 1))
    else (firstChar, secondChar, (max top left))

let findLCSLenSeq (topString: array<char>) (sideString: array<char>) (topValues: array<int>) (sideValues: array<int>) = 
    (*let vectorOne = Array.zeroCreate(topString.Length)
    let vectorTwo = Array.zeroCreate(topString.Length)
    let vectorThree = Array.zeroCreate(topString.Length)*)

    //Arrays first part of array is position in the vector, second is x co-ord, y co-ord, cell value
    (*let vectorOne = Array2D.zeroCreate topString.Length 3
    let vectorTwo = Array2D.zeroCreate topString.Length 3
    let vectorThree = Array2D.zeroCreate topString.Length 3*)

    let vectorOne = Array2D.init topString.Length 3 (fun x y -> None)
    let vectorTwo = Array2D.init topString.Length 3 (fun x y -> None)
    let vectorThree = Array2D.init topString.Length 3 (fun x y -> None)

    //Initialise vector values
    //vectorOne.[0] <- (topIndex, sideIndex, topValues)
   (* vectorOne.[0] <- (0, 0, topValues.[1])
    vectorTwo.[0] <- (0, -1, 0)
    vectorTwo.[1] <- (-1, 0, 0)
    vectorThree.[0] <- (-1, -1, 0) *)

    //Initialise vectors two and three
    vectorTwo.[0, 0] <- 0
    vectorTwo.[0, 1] <- 1
    vectorTwo.[0, 2] <- topValues.[1]
    vectorTwo.[1, 0] <- 1
    vectorTwo.[1, 1] <- 0
    vectorTwo.[1, 2] <- sideValues.[1]
    vectorThree.[0, 0] <- 0
    vectorThree.[0, 1] <- 0
    vectorThree.[0, 2] <- sideValues.[0]

    Array2D.iter (fun x -> Some(x)) vectorTwo.[2.., *]

    //First step
    //vectorOne.GetValue(0)
    64


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