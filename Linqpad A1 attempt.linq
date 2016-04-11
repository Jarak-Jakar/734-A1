<Query Kind="FSharpProgram">
  <Reference Relative="packages\Akka.1.0.6\lib\net45\Akka.dll">H:\2016\Semester One\CS 734\Assignment One\A1\packages\Akka.1.0.6\lib\net45\Akka.dll</Reference>
  <Reference Relative="packages\Akka.FSharp.1.0.6\lib\net45\Akka.FSharp.dll">H:\2016\Semester One\CS 734\Assignment One\A1\packages\Akka.FSharp.1.0.6\lib\net45\Akka.FSharp.dll</Reference>
</Query>

open Akka
open Akka.FSharp


let checkCell firstChar secondChar top left topLeft = 
    if firstChar = secondChar then (firstChar, secondChar, (topLeft + 1))
    else (firstChar, secondChar, (max top left))

let findLCSLenSeq (topString: array<char>) (sideString: array<char>) (topValues: array<int>) (sideValues: array<int>) = 
    (*let vectorOne = Array2D.create topString.Length 3 0
    let vectorTwo = Array2D.create topString.Length 3 0
    let vectorThree = Array2D.create topString.Length 3 0*)

    let vectorOne = Array.create topString.Length (Some([|0; 0; 0|]))
    let vectorTwo = Array.create topString.Length (Some([|0; 0; 0|]))
    let vectorThree = Array.create topString.Length (Some([|0; 0; 0|]))

    //Initialise vectors
    

    64


[<EntUtil.Cmd (@"");ryPoint>]
let main argv = 
    printfn "%A" argv
    printfn "Hello world!"
    printfn "%A" argv.[2]
    let stringOne = System.IO.File.ReadAllText(argv.[0].[4..]).ToCharArray()
    let stringTwo = System.IO.File.ReadAllText(argv.[1].[4..]).ToCharArray()
    //printfn "%A" stringOne
    //printfn "%A" stringTwo
    //printfn "%A" (findLCSLenSeq stringOne stringTwo 3 4)
    let LCSLen = if stringOne.Length > stringTwo.Length then findLCSLenSeq stringTwo stringOne (Array.zeroCreate(stringTwo.Length + 1)) (Array.zeroCreate(stringOne.Length + 1))
                 else findLCSLenSeq stringOne stringTwo (Array.zeroCreate(stringOne.Length + 1)) (Array.zeroCreate(stringTwo.Length + 1))
    printfn "%A" LCSLen
    0 // return an integer exit code