// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open Akka
open Akka.FSharp


let checkCell firstChar secondChar top left topLeft = 
    //printfn "\nIn checkCell"
    //printfn "firstChar: %A, secondChar: %A, top: %A, left: %A, topLeft: %A\n" firstChar secondChar top left topLeft
    if firstChar = secondChar then topLeft + 1
    else max top left

(*let initVector startNum arrayInit = 
    Array.iteri (fun i (x: array<int>) -> 
        x.[0] <- startNum - i
        x.[1] <- i
        x.[2] <- -1) arrayInit*)

(*let initVector startNum (arrayInit: int[,]) = 
    Array2D.iteri (fun i j x -> 
        match j with
        | 0 -> arrayInit.[i,j] <- startNum - i
        | 1 -> arrayInit.[i,j] <- i
        | _ -> arrayInit.[i,j] <- -1) arrayInit*)

let findLCSLenSeq (topString: array<char>) (sideString: array<char>) (topValues: array<int>) (sideValues: array<int>) = 

    let vectorOne = Array2D.create topString.Length 3 0
    let vectorTwo = Array2D.create topString.Length 3 0
    let vectorThree = Array2D.create topString.Length 3 0

    (*printfn "vectorOne: %A" vectorOne
    printfn "vectorTwo: %A" vectorTwo
    printfn "vectorThree: %A" vectorThree*)

    let initVector startNum (arrayInit: int[,]) = 
        Array2D.iteri (fun i j x -> 
            match j with
            | 0 -> arrayInit.[i,j] <- startNum - i
            | 1 -> arrayInit.[i,j] <- i
            | _ -> arrayInit.[i,j] <- -1) arrayInit

    initVector 2 vectorOne
    initVector 1 vectorTwo
    initVector 0 vectorThree

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

    let v2flen = Array2D.length1 vectorOne // Creating the index comparison value for the below function, but since it always operates on vectorOne (which of course has constant size) it shouldn't be an issue
    
    // There's surely a better way to do this bit than all these if statements, but I don't know it and haven't figured it out yet
    let rec fillVectorOne (vectorToFill: int[,]) index =
        if index < (v2flen) then // Stop when this has gone over the whole vector array
            if (vectorToFill.[index, 0] >= sideValues.Length) || (vectorToFill.[index, 1] >= topValues.Length) then index |> ignore // If the cell's locale is outside the top or right, then just move on to the next cell
            elif (vectorToFill.[index, 0] < 0) || (vectorToFill.[index, 1] < 0) then fillVectorOne vectorToFill (Array2D.length1 vectorToFill) // If the cell's locale is outside the left or bottom side of the table, stop recursing by jumping to the end condition
            elif vectorToFill.[index, 0] = 0 then vectorToFill.[index, 2] <- sideValues.[vectorToFill.[index, 1]] // If it is on the side, take the value from the side values array
            elif vectorToFill.[index, 1] = 0 then vectorToFill.[index, 2] <- topValues.[vectorToFill.[index, 0]] // If it is on the top, take the value from the top values array
            elif topString.[vectorToFill.[index, 0] - 1] = sideString.[vectorToFill.[index, 1] - 1] then vectorToFill.[index, 2] <- (vectorThree.[index - 1, 2] + 1) // Actually perform the calculation function
            else vectorToFill.[index, 2] <- max vectorTwo.[index - 1, 2] vectorTwo.[index, 2]  // Otherwise, take the max and move on
            fillVectorOne vectorToFill (index + 1) // Recurse to go around again - this should be tail recursion so it shouldn't be too much of an issue

    //fillVectorOne vectorOne 0

    (*printfn "vectorOne: %A" vectorOne
    printfn "vectorTwo: %A" vectorTwo
    printfn "vectorThree: %A" vectorThree*)
    
    // Kinda ugly, and probably not strictly functional, but should be a relatively efficient way to roll the vectors forward
    //System.Array.Copy(vectorTwo, vectorThree, vectorThree.Length)
    //System.Array.Copy(vectorOne, vectorTwo, vectorTwo.Length)

    // Now that vectorTwo and vectorThree have been rolled forward, update the positions in vectorOne
    let updateVector startNum (arrayUpdate: int[,]) =
        Array2D.iteri (fun i j x ->
            match j with
            | 0 -> arrayUpdate.[i, j] <- startNum - i
            | 1 -> arrayUpdate.[i, j] <- i
            | _ -> x |> ignore) arrayUpdate

    //updateVector (vectorOne.[0, 0] + 1) vectorOne

    let tVLsVLMO = topValues.Length + sideValues.Length - 1 // Calculate this number, which will be constant, to avoid recalculation in the for loop below

    for i = 2 to tVLsVLMO do
        fillVectorOne vectorOne 0
        System.Array.Copy(vectorTwo, vectorThree, vectorThree.Length)
        System.Array.Copy(vectorOne, vectorTwo, vectorTwo.Length)
        updateVector (vectorOne.[0, 0] + 1) vectorOne
        (*printfn "vectorOne: %A" vectorOne
        printfn "vectorTwo: %A" vectorTwo
        printfn "vectorThree: %A" vectorThree*)

    (*printfn "vectorOne: %A" vectorOne
    printfn "vectorTwo: %A" vectorTwo
    printfn "vectorThree: %A" vectorThree*)

    vectorOne.[(Array2D.length1 vectorOne) - 1 , 2] // The final return value


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