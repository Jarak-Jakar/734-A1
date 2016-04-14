// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open Akka
open Akka.FSharp


(*let checkCell firstChar secondChar top left topLeft = 
    //printfn "\nIn checkCell"
    //printfn "firstChar: %A, secondChar: %A, top: %A, left: %A, topLeft: %A\n" firstChar secondChar top left topLeft
    if firstChar = secondChar then topLeft + 1
    else max top left*)

let findLCSLenSeq' (topString: array<char>) (sideString: array<char>) (topValues: array<int>) (sideValues: array<int>) = //Going to ignore this one and start again for now

    let vectorLength = sideString.Length

    let vectorOne = Array2D.init vectorLength 3 (fun i j -> 
        match j with
        | 0 -> 2 - i
        | 1 -> i
        | _ -> -1 )

    let vectorTwo = Array2D.init vectorLength 3 (fun i j -> 
        match j with
        | 0 -> 1 - i
        | 1 -> i
        | _ -> -1 )

    let vectorThree = Array2D.init vectorLength 3 (fun i j -> 
        match j with
        | 0 -> 0 - i
        | 1 -> i
        | _ -> -1 )

    (*printfn "vectorOne: %A" vectorOne
    printfn "vectorTwo: %A" vectorTwo
    printfn "vectorThree: %A" vectorThree*)

    // Prepopulate vectorTwo and vectorThree with some important initial values
    vectorTwo.[0,2] <- topValues.[1]
    vectorTwo.[1,2] <- sideValues.[1]
    vectorThree.[0,2] <- topValues.[0]

    (*printfn "vectorOne: %A" vectorOne
    printfn "vectorTwo: %A" vectorTwo
    printfn "vectorThree: %A" vectorThree*)

    // There's surely a better way to do the bounds checking than all these if statements, but I don't know it and haven't figured it out
    (*let rec fillVectorOne (vectorToFill: int[,]) index =
        if index < (vectorLength) then // Stop when this has gone over the whole vector array
            if (vectorToFill.[index, 0] >= sideValues.Length) || (vectorToFill.[index, 1] >= topValues.Length) then index |> ignore // If the cell's locale is outside the top or right, then just move on to the next cell
            //elif (vectorToFill.[index, 0] < 0) || (vectorToFill.[index, 1] < 0) then fillVectorOne vectorToFill (Array2D.length1 vectorToFill) // If the cell's locale is outside the left or bottom side of the table, stop recursing by jumping to the end condition
            elif (vectorToFill.[index, 0] < 0) || (vectorToFill.[index, 1] < 0) then index |> ignore // If the cell's locale is outside the left or bottom side of the table, then just move on to the next cell
            //if vectorToFill.[index, 0] = 0 then vectorToFill.[index, 2] <- sideValues.[vectorToFill.[index, 1]] // If it is on the side, take the value from the side values array
            elif vectorToFill.[index, 0] = 0 then vectorToFill.[index, 2] <- sideValues.[vectorToFill.[index, 1]] // If it is on the side, take the value from the side values array
            elif vectorToFill.[index, 1] = 0 then vectorToFill.[index, 2] <- topValues.[vectorToFill.[index, 0]] // If it is on the top, take the value from the top values array
            //elif (vectorToFill.[index, 0] >= sideValues.Length) || (vectorToFill.[index, 1] >= topValues.Length) then fillVectorOne vectorToFill (index + 1)
            //elif (vectorToFill.[index, 0] < 0) || (vectorToFill.[index, 1] < 0) then fillVectorOne vectorToFill (Array2D.length1 vectorToFill) // If the cell's locale is outside the left or bottom side of the table, stop recursing by jumping to the end condition
            //else vectorToFill.[index, 2] <- checkCell topString.[vectorToFill.[index, 0] - 1] sideString.[vectorToFill.[index, 1] - 1] vectorTwo.[index - 1, 2] vectorTwo.[index, 2] vectorThree.[index - 1, 2] // Otherwise, actually perform the calculation function
            else vectorToFill.[index, 2] <- checkCell topString.[vectorToFill.[index, 0] - 1] sideString.[vectorToFill.[index, 1] - 1] vectorTwo.[index - 1, 2] vectorTwo.[index, 2] vectorThree.[index - 1, 2] // Otherwise, actually perform the calculation function

            // Do some vector maintenance, in preparation for processing the next cell (and the next step of the vector)
            //Transfer vector two back to vector three
            (*vectorThree.[index, 0] <- vectorTwo.[index, 0]
            vectorThree.[index, 1] <- vectorTwo.[index, 1]
            vectorThree.[index, 2] <- vectorTwo.[index, 2]*)

            // Transfer vector one back to vector two
            (*vectorTwo.[index, 0] <- vectorOne.[index, 0]
            vectorTwo.[index, 1] <- vectorOne.[index, 1]
            vectorTwo.[index, 2] <- vectorOne.[index, 2]*)

            // Update the values in vectorOne in preparation for the next fillVector
            vectorOne.[index, 0] <- (vectorOne.[index, 0] + 1)
            vectorOne.[index, 1] <- index

            fillVectorOne vectorToFill (index + 1) // Recurse to go around again

    //fillVectorOne vectorOne 0

    printfn "vectorOne: %A" vectorOne
    printfn "vectorTwo: %A" vectorTwo
    printfn "vectorThree: %A" vectorThree
    
    // Kinda ugly, and probably not strictly functional, but should be a relatively efficient way to roll the vectors forward
    //System.Array.Copy(vectorTwo, vectorThree, vectorThree.Length)
    //System.Array.Copy(vectorOne, vectorTwo, vectorTwo.Length)

    // Now that vectorTwo and vectorThree have been rolled forward, update the positions in vectorOne
    (*let updateVector startNum (arrayUpdate: int[,]) =
        Array2D.iteri (fun i j x ->
            match j with
            | 0 -> arrayUpdate.[i, j] <- startNum - i
            | 1 -> arrayUpdate.[i, j] <- i
            | _ -> x |> ignore) arrayUpdate*)

    //updateVector (vectorOne.[0, 0] + 1) vectorOne

    for i = 2 to (topValues.Length + sideValues.Length - 1) do
        fillVectorOne vectorOne 0
        System.Array.Copy(vectorTwo, vectorThree, vectorThree.Length)
        System.Array.Copy(vectorOne, vectorTwo, vectorTwo.Length)
        //updateVector (vectorOne.[0, 0] + 1) vectorOne
        (*printfn "vectorOne: %A" vectorOne
        printfn "vectorTwo: %A" vectorTwo
        printfn "vectorThree: %A" vectorThree*)

    (*printfn "vectorOne: %A" vectorOne
    printfn "vectorTwo: %A" vectorTwo
    printfn "vectorThree: %A" vectorThree*)*)



    //vectorOne.[(vectorLength) - 1 , 2] // The final return value

    let rec fillVectorOne index = 
        if index < vectorLength then
            if (vectorOne.[index, 0] < 0) || (vectorOne.[index, 1] < 0) then fillVectorOne(vectorLength) // Skip to end if have reached part of vector outside table
            else vectorOne.[index, 2] <- 
                    if topString.[vectorOne.[index, 0] - 1] = sideString.[vectorOne.[index, 1] - 1] then vectorThree.[index - 1, 2]
                    else max vectorTwo.[index, 2] vectorTwo.[index - 1, 2]

        fillVectorOne (index + 1)  // Should be a tail-recursive call

    64

let findLCSLenSeq (topString: char[]) (sideString: char[]) (topValues: int[]) (sideValues: int[]) = 
    
    let vectorLength = topValues.Length
    
    let mutable vectorOne = Array.create vectorLength 0
    let mutable vectorTwo = Array.create vectorLength 0
    let mutable vectorThree = Array.create vectorLength 0

    // Initialise the back vectors
    vectorThree.[0] <- topValues.[0]
    vectorTwo.[0] <- topValues.[1]
    vectorTwo.[1] <- sideValues.[1]

    // Function to traverse a vector and fill it with the relevant values
    let rec fillVector index finalIndex iterationNum (frontVector: int[]) (middleVector: int[]) (backVector: int[]) = 
        if index < finalIndex then  // Stop if have gone outside the bounds provided
            printfn "Entered fillVector!"
            if topString.[iterationNum - index] = sideString.[index] then frontVector.[index] <- backVector.[index - 1] + 1
            else frontVector.[index] <- max middleVector.[index] middleVector.[index - 1]
            fillVector (index + 1) finalIndex iterationNum frontVector middleVector backVector

    // Traverse over the first part of the array, while the vectors build up to full size
    (*for i = 2 to (sideString.Length - 1) do
        vectorOne.[0] <- topValues.[i]
        fillVector 1 (i - 1) i vectorOne vectorTwo vectorThree
        vectorOne.[i] <- sideValues.[i]
        printfn "vectorOne: %A\nvectorTwo: %A\nvectorThree:%A" vectorOne vectorTwo vectorThree*)

    let mutable traversalIndex = 2

    let rec traverseFirstSection tIndex (firstVector: int[]) (secondVector: int[]) (thirdVector: int[]) =
        if tIndex < sideString.Length then
            firstVector.[0] <- topValues.[tIndex]
            fillVector 1 tIndex tIndex firstVector secondVector thirdVector
            firstVector.[tIndex] <- sideValues.[tIndex]
            printfn "firstVector: %A\nsecondVector: %A\nthirdVector:%A" firstVector secondVector thirdVector
            traverseFirstSection (tIndex + 1) secondVector thirdVector firstVector
        vectorThree <- thirdVector
        vectorTwo <- secondVector
        vectorOne <- firstVector
        traversalIndex <- tIndex
        printfn "vectorOne: %A\nvectorTwo: %A\nvectorThree: %A\ntraversalIndex: %A" vectorOne vectorTwo vectorThree traversalIndex
        

    traverseFirstSection traversalIndex vectorOne vectorTwo vectorThree

    64

[<EntryPoint>]
let main argv = 
    //printfn "%A" argv
    //printfn "Hello world!"
    printfn "%A" argv.[2]
    let stringOne = System.IO.File.ReadAllText(argv.[0].[4..]).ToCharArray()
    let stringTwo = System.IO.File.ReadAllText(argv.[1].[4..]).ToCharArray()
    //printfn "%A" stringOne
    //printfn "%A" stringTwo
    //printfn "%A" (findLCSLenSeq stringOne stringTwo 3 4)
    let LCSLen = if stringOne.Length < stringTwo.Length then findLCSLenSeq stringTwo stringOne (Array.zeroCreate(stringTwo.Length + 1)) (Array.zeroCreate(stringOne.Length + 1))
                 else findLCSLenSeq stringOne stringTwo (Array.zeroCreate(stringOne.Length + 1)) (Array.zeroCreate(stringTwo.Length + 1))
    printfn "%A" LCSLen
    0 // return an integer exit code