// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open Akka
open Akka.FSharp

let findLCSLenSeq (topString: char[]) (sideString: char[]) (topValues: int[]) (sideValues: int[]) = // This seems to work pretty well...
    
    let vectorLength = sideValues.Length
    
    let mutable vectorOne = Array.create vectorLength 0
    let mutable vectorTwo = Array.create vectorLength 0
    let mutable vectorThree = Array.create vectorLength 0
    let bottomValues = Array.create topValues.Length 0
    let rightValues = Array.create vectorLength 0
    let mutable tempVector = Array.create vectorLength 0 // This one will just be used to temporarily hold the pointer for one of the other vectors, declaring it here so that it won't be
                                                         // Garbage Collected until the end of findLCSLenSeq

    // Initialise the back vectors
    vectorThree.[0] <- topValues.[0]
    vectorTwo.[0] <- topValues.[1]
    vectorTwo.[1] <- sideValues.[1]

    // Function to traverse a vector and fill it with the relevant values
    let rec fillVector index finalIndex iterationNum (frontVector: int[]) (middleVector: int[]) (backVector: int[]) = 
        //printfn "Entered fillVector!  index: %A, finalIndex: %A, iterationNum %A" index finalIndex iterationNum
        if index < finalIndex then  // Stop if have gone outside the bounds provided
            //printfn "topString: %A, sideString: %A" topString.[iterationNum - index - 1] sideString.[index - 1]
            if topString.[iterationNum - index - 1] = sideString.[index - 1] then frontVector.[index] <- backVector.[index - 1] + 1
            else frontVector.[index] <- max middleVector.[index] middleVector.[index - 1]
            fillVector (index + 1) finalIndex iterationNum frontVector middleVector backVector

    // Traverse over the first part of the array, while the vectors build up to full size
    for i = 2 to sideString.Length do
        vectorOne.[0] <- topValues.[i]
        fillVector 1 i i vectorOne vectorTwo vectorThree
        vectorOne.[i] <- sideValues.[i]
        //printfn "vectorOne: %A\nvectorTwo: %A\nvectorThree:%A" vectorOne vectorTwo vectorThree
        tempVector <- vectorThree  // Exchange pointers for the arrays - this lets me efficiently reuse the same arrays in memory
        vectorThree <- vectorTwo  // Which reduces garbage collection frequency significantly, and avoids expensive array copying functions
        vectorTwo <- vectorOne
        vectorOne <- tempVector

    bottomValues.[0] <- sideValues.[sideValues.Length - 1]

    // Traverse over the middle part, where the vector will be at full length - this may not necessarily be triggered
    for i = (sideString.Length + 1) to topString.Length do
        //printfn "i = %A" i
        vectorOne.[0] <- topValues.[i]
        fillVector 1 (sideString.Length + 1) i vectorOne vectorTwo vectorThree
        bottomValues.[i - vectorLength + 1] <- vectorOne.[vectorLength - 1]
        //printfn "vectorOne: %A\nvectorTwo: %A\nvectorThree:%A\nbottomValues: %A" vectorOne vectorTwo vectorThree bottomValues
        tempVector <- vectorThree
        vectorThree <- vectorTwo
        vectorTwo <- vectorOne
        vectorOne <- tempVector

    rightValues.[0] <- topValues.[topValues.Length - 1]

    // Traverse over the last part, where the vector will be slowly reducing
    for i = (topString.Length + 1) to (topString.Length + sideString.Length) do
        fillVector (i - topString.Length) vectorLength i vectorOne vectorTwo vectorThree
        rightValues.[i - topString.Length] <- vectorOne.[i - topString.Length]
        bottomValues.[i - vectorLength + 1] <- vectorOne.[vectorLength - 1]
        //printfn "vectorOne: %A\nvectorTwo: %A\nvectorThree:%A\nbottomValues: %A\nrightValues: %A" vectorOne vectorTwo vectorThree bottomValues rightValues
        tempVector <- vectorThree
        vectorThree <- vectorTwo
        vectorTwo <- vectorOne
        vectorOne <- tempVector

    vectorTwo.[vectorTwo.Length - 1]  // vectorTwo here, because I will have just swapped the pointers around so that this is now what was vectorOne

[<EntryPoint>]
let main argv = 
    //printfn "%A" argv
    //printfn "Hello world!"
    //printfn "%A" argv.[2]
    let stringOne = System.IO.File.ReadAllText(argv.[0].[4..]).ToCharArray()
    let stringTwo = System.IO.File.ReadAllText(argv.[1].[4..]).ToCharArray()
    let mutable LCSLen = 0
    let mode = argv.[2]
    match mode with
        | "/SEQ" -> LCSLen <- if stringOne.Length < stringTwo.Length then findLCSLenSeq stringTwo stringOne (Array.zeroCreate(stringTwo.Length + 1)) (Array.zeroCreate(stringOne.Length + 1))
                              else findLCSLenSeq stringOne stringTwo (Array.zeroCreate(stringOne.Length + 1)) (Array.zeroCreate(stringTwo.Length + 1))
        | "/PAR-SYNC" -> LCSLen <- -1
        | "/PAR-ASYNC" -> LCSLen <- -2
        | _ -> printfn "Incorrect mode parameter stated"
    //printfn "%A" stringOne
    //printfn "%A" stringTwo
    //printfn "%A" (findLCSLenSeq stringOne stringTwo 3 4)
    //LCSLen <- if stringOne.Length < stringTwo.Length then findLCSLenSeq stringTwo stringOne (Array.zeroCreate(stringTwo.Length + 1)) (Array.zeroCreate(stringOne.Length + 1))
    //          else findLCSLenSeq stringOne stringTwo (Array.zeroCreate(stringOne.Length + 1)) (Array.zeroCreate(stringTwo.Length + 1))
    printfn "%d" LCSLen
    0 // return an integer exit code