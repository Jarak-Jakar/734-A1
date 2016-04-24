// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open FSharp.Core
open Akka
open Akka.Actor
open Akka.FSharp


type agentMessage =
    | TopStr of char[]
    | SideStr of char[]
    | TopVals of int[]
    | SideVals of int[]

let findLCSLenSeq (topString: char[]) (sideString: char[]) (topValues: int[]) (sideValues: int[]) = // This seems to work pretty well...
                                                                                                    // Though note that it assumes that topString >= sideString
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

    (vectorTwo.[vectorTwo.Length - 1], bottomValues, rightValues)  // vectorTwo here, because I will have just swapped the pointers around so that this is now what was vectorOne

let parsync (topString: char[]) (sideString: char[]) (topValues: int[]) (sideValues: int[]) verticalChunks horizontalChunks = 
    // Start by breaking the arrays up into the called for chunks

    let topStringsLength = int (round (float topString.Length / float verticalChunks))
    let sideStringsLength = int (round (float sideString.Length / float horizontalChunks))
    let barrier = new System.Threading.Barrier (2)
    let mutable returnValue = 0  // Using this relies on the fact that the last actor to execute is the one in the bottom right corner
    
    // Not great because it actually creates copies of the array subsections, but I'll use it anyway...
    let topStringsArray = Array.create verticalChunks [|'a'|]
    let topValsArray = Array.create verticalChunks [|0|]
    if verticalChunks = 1 then
        topStringsArray.[0] <- topString
        topValsArray.[0] <- topValues
    else
        topStringsArray.[0] <- Array.sub topString 0 topStringsLength
        topValsArray.[0] <- Array.sub topValues 0 (topStringsLength + 1)
        for i = 1 to (verticalChunks - 2) do
            topStringsArray.[i] <- Array.sub topString (i * topStringsLength) topStringsLength
            topValsArray.[i] <- Array.sub topValues ((i * topStringsLength) - 1) (topStringsLength + 1)
        topStringsArray.[(verticalChunks - 1)] <- Array.sub topString ((verticalChunks - 1) * topStringsLength) (topString.Length - ((verticalChunks - 1) * topStringsLength))
        topValsArray.[(verticalChunks - 1)] <- Array.sub topValues (((verticalChunks - 1) * topStringsLength) - 1) (topStringsArray.[(verticalChunks - 1)].Length + 1) //(topString.Length - ((verticalChunks - 1) * topStringsLength) + 1)

    let sideStringsArray = Array.create horizontalChunks [|'a'|]
    let sideValsArray = Array.create horizontalChunks [|0|]
    if horizontalChunks = 1 then
        sideStringsArray.[0] <- sideString
        sideValsArray.[0] <- sideValues
    else
        sideStringsArray.[0] <- Array.sub sideString 0 sideStringsLength
        sideValsArray.[0] <- Array.sub sideValues 0 (sideStringsLength + 1)
        for i = 1 to (horizontalChunks - 2) do
            sideStringsArray.[i] <- Array.sub sideString (i * sideStringsLength) sideStringsLength
            sideValsArray.[i] <- Array.sub sideValues ((i * sideStringsLength) - 1) (sideStringsLength + 1)
        sideStringsArray.[(horizontalChunks - 1)] <- Array.sub sideString ((horizontalChunks - 1) * sideStringsLength) (sideString.Length - ((horizontalChunks - 1) * sideStringsLength))
        sideValsArray.[(horizontalChunks - 1)] <- Array.sub sideValues (((horizontalChunks - 1) * sideStringsLength) - 1) (sideStringsArray.[(horizontalChunks - 1)].Length + 1)

    if verticalChunks >= horizontalChunks then // For all cases where there are at least as many divisions along the x-axis as along the y-axis
        // Do process
        use system = ActorSystem.Create "734AssignmentOne"
        let agents = Array.create horizontalChunks (spawn system "actorName" (actorOf (fun msg -> ())))
        // This will probably have a huge performance penalty, since it is initialising actors with a blank function...

    // Define the actor type to be used here
        let createAgent numInArray = 
            spawn system (sprintf "agentLCS%d" numInArray)
            <| fun inbox ->
                let mutable tStrChunk = [|'a'|]
                let mutable sStrChunk = [|'a'|]
                let mutable tValChunk = [|-1|]
                let mutable sValChunk = [|-1|]
                let rec loop n = 
                    actor {                    
                        let! mess1 = inbox.Receive ()
                        match mess1 with
                        | TopStr x -> tStrChunk <- x
                        | SideStr x -> sStrChunk <- x
                        | TopVals x -> tValChunk <- x
                        | SideVals x -> sValChunk <- x

                        let! mess2 = inbox.Receive ()
                        match mess2 with
                        | TopStr x -> tStrChunk <- x
                        | SideStr x -> sStrChunk <- x
                        | TopVals x -> tValChunk <- x
                        | SideVals x -> sValChunk <- x

                        let! mess3 = inbox.Receive ()
                        match mess3 with
                        | TopStr x -> tStrChunk <- x
                        | SideStr x -> sStrChunk <- x
                        | TopVals x -> tValChunk <- x
                        | SideVals x -> sValChunk <- x

                        let! mess4 = inbox.Receive ()
                        match mess4 with
                        | TopStr x -> tStrChunk <- x
                        | SideStr x -> sStrChunk <- x
                        | TopVals x -> tValChunk <- x
                        | SideVals x -> sValChunk <- x

                        if sValChunk.Length > tValChunk.Length then
                            let (len, rVals, bVals) = findLCSLenSeq sStrChunk tStrChunk sValChunk tValChunk
                            returnValue <- len
                            if numInArray < (horizontalChunks - 1) then
                                agents.[numInArray + 1] <! (TopVals bVals)

                            if n < verticalChunks then
                                agents.[numInArray] <! (SideVals rVals)
                        else 
                            let (len, bVals, rVals) = findLCSLenSeq tStrChunk sStrChunk tValChunk sValChunk
                            returnValue <- len
                            if numInArray < (horizontalChunks - 1) then
                                agents.[numInArray + 1] <! (TopVals bVals)

                            if n < verticalChunks then
                                agents.[numInArray] <! (SideVals rVals)

                        //printfn "%d %d %d" numInArray n returnValue
                        System.Console.WriteLine("{0} {1} {2}", numInArray, n, returnValue)

                        barrier.SignalAndWait ()

                        return! loop (n + 1)
                    }
                loop 0

        for i = 0 to (agents.Length - 1) do  // Initialise agents with correct function
            agents.[i] <- createAgent i

        // Now loop over the table, triggering actors
        for i = 0 to ((min verticalChunks horizontalChunks) - 1) do
            agents.[0] <! (TopVals topValsArray.[i])
            for j in i .. -1 .. 0 do
                agents.[j] <! (TopStr topStringsArray.[i - j])
                agents.[j] <! (SideStr sideStringsArray.[j])
                if (i - j) = 0 then
                    agents.[j] <! (SideVals sideValsArray.[j])
            barrier.SignalAndWait ()
            barrier.AddParticipant () |> ignore

        barrier.RemoveParticipant () // Stupid, but necessary due to the way the above works...

        // Now loop over the 'middle' section, if there are more chunks along the top than the side
        for i = (min verticalChunks horizontalChunks) to (verticalChunks - 1) do
            agents.[0] <! (TopVals topValsArray.[i])
            for j = 0 to (agents.Length - 1) do
                agents.[j] <! (TopStr topStringsArray.[i - j])
                agents.[j] <! (SideStr sideStringsArray.[j])
            barrier.SignalAndWait ()

        for i = verticalChunks to (verticalChunks + horizontalChunks - 1) do
            barrier.RemoveParticipant ()
            for j = (i - verticalChunks + 1) to (agents.Length - 1) do
                agents.[j] <! (TopStr topStringsArray.[i  - j])
                agents.[j] <! (SideStr sideStringsArray.[j])
            barrier.SignalAndWait ()
        returnValue

    else
        // Do process
        use system = ActorSystem.Create "734AssignmentOne"
        let agents = Array.create horizontalChunks (spawn system "actorName" (actorOf (fun msg -> ())))
        // This will probably have a huge performance penalty, since it is initialising actors with a blank function...

    // Define the actor type to be used here
        let createAgent numInArray = 
            spawn system (sprintf "agentLCS%d" numInArray)
            <| fun inbox ->
                let mutable tStrChunk = [|'a'|]
                let mutable sStrChunk = [|'a'|]
                let mutable tValChunk = [|-1|]
                let mutable sValChunk = [|-1|]
                let rec loop n = 
                    actor {                    
                        let! mess1 = inbox.Receive ()
                        match mess1 with
                        | TopStr x -> tStrChunk <- x
                        | SideStr x -> sStrChunk <- x
                        | TopVals x -> tValChunk <- x
                        | SideVals x -> sValChunk <- x

                        let! mess2 = inbox.Receive ()
                        match mess2 with
                        | TopStr x -> tStrChunk <- x
                        | SideStr x -> sStrChunk <- x
                        | TopVals x -> tValChunk <- x
                        | SideVals x -> sValChunk <- x

                        let! mess3 = inbox.Receive ()
                        match mess3 with
                        | TopStr x -> tStrChunk <- x
                        | SideStr x -> sStrChunk <- x
                        | TopVals x -> tValChunk <- x
                        | SideVals x -> sValChunk <- x

                        let! mess4 = inbox.Receive ()
                        match mess4 with
                        | TopStr x -> tStrChunk <- x
                        | SideStr x -> sStrChunk <- x
                        | TopVals x -> tValChunk <- x
                        | SideVals x -> sValChunk <- x

                        if sValChunk.Length > tValChunk.Length then
                            let (len, rVals, bVals) = findLCSLenSeq sStrChunk tStrChunk sValChunk tValChunk
                            returnValue <- len
                            if numInArray < (horizontalChunks - 1) then
                                agents.[numInArray + 1] <! (TopVals bVals)

                            if n < verticalChunks then
                                agents.[numInArray] <! (SideVals rVals)
                        else 
                            let (len, bVals, rVals) = findLCSLenSeq tStrChunk sStrChunk tValChunk sValChunk
                            returnValue <- len
                            if numInArray < (horizontalChunks - 1) then
                                agents.[numInArray + 1] <! (TopVals bVals)

                            if n < verticalChunks then
                                agents.[numInArray] <! (SideVals rVals)

                        //printfn "%d %d %d" numInArray n returnValue
                        System.Console.WriteLine("{0} {1} {2}", numInArray, n, returnValue)

                        barrier.SignalAndWait ()

                        return! loop (n + 1)
                    }
                loop 0

        for i = 0 to (agents.Length - 1) do  // Initialise agents with correct function
            agents.[i] <- createAgent i

        // Now loop over the table, triggering actors (?)
        for i = 0 to ((min verticalChunks horizontalChunks) - 1) do
            agents.[0] <! (TopVals topValsArray.[i])
            for j = 0 to i do
                agents.[j] <! (TopStr topStringsArray.[i - j])
                agents.[j] <! (SideStr sideStringsArray.[j])
                if (i - j) = 0 then
                    agents.[j] <! (SideVals sideValsArray.[j])
            barrier.SignalAndWait ()
            barrier.AddParticipant () |> ignore

        barrier.RemoveParticipant () // Stupid, but necessary due to the way the above works...

        // Now loop over the 'middle' section, if there are more chunks along the side than the top
        for i = (min verticalChunks horizontalChunks) to (horizontalChunks - 1) do
            for j = (i - verticalChunks + 1) to i do
                  agents.[j] <! (TopStr topStringsArray.[i - j])
                  agents.[j] <! (SideStr sideStringsArray.[j])
            agents.[i] <! (SideVals sideValsArray.[i])
            barrier.SignalAndWait ()
        
        // And now do the end part
        for i = horizontalChunks to (verticalChunks + horizontalChunks - 1) do
            barrier.RemoveParticipant ()
            for j = (i - verticalChunks + 1) to (agents.Length - 1) do
                agents.[j] <! (TopStr topStringsArray.[i  - j])
                agents.[j] <! (SideStr sideStringsArray.[j])
            barrier.SignalAndWait ()
        returnValue

let parasync (topString: char[]) (sideString: char[]) (topValues: int[]) (sideValues: int[]) verticalChunks horizontalChunks = 
    
    let topStringsLength = int (round (float topString.Length / float verticalChunks))
    let sideStringsLength = int (round (float sideString.Length / float horizontalChunks))

    // Whichever length is greater determines which way around sideString and topString are supplied to the find function, due to its assumption that top >= side
    
    // Not great because it actually creates copies of the array subsections, but I'll use it anyway...

    let topStringsArray = Array.create verticalChunks [|'a'|]
    let topValsArray = Array.create verticalChunks [|0|]
    if verticalChunks = 1 then
        topStringsArray.[0] <- topString
        topValsArray.[0] <- topValues
    else
        topStringsArray.[0] <- Array.sub topString 0 topStringsLength
        topValsArray.[0] <- Array.sub topValues 0 (topStringsLength + 1)
        for i = 1 to (verticalChunks - 2) do
            topStringsArray.[i] <- Array.sub topString (i * topStringsLength) topStringsLength
            topValsArray.[i] <- Array.sub topValues ((i * topStringsLength) - 1) (topStringsLength + 1)
        topStringsArray.[(verticalChunks - 1)] <- Array.sub topString ((verticalChunks - 1) * topStringsLength) (topString.Length - ((verticalChunks - 1) * topStringsLength))
        topValsArray.[(verticalChunks - 1)] <- Array.sub topValues (((verticalChunks - 1) * topStringsLength) - 1) (topStringsArray.[(verticalChunks - 1)].Length + 1) //(topString.Length - ((verticalChunks - 1) * topStringsLength) + 1)

    let sideStringsArray = Array.create horizontalChunks [|'a'|]
    let sideValsArray = Array.create horizontalChunks [|0|]
    if horizontalChunks = 1 then
        sideStringsArray.[0] <- sideString
        sideValsArray.[0] <- sideValues
    else
        sideStringsArray.[0] <- Array.sub sideString 0 sideStringsLength
        sideValsArray.[0] <- Array.sub sideValues 0 (sideStringsLength + 1)
        for i = 1 to (horizontalChunks - 2) do
            sideStringsArray.[i] <- Array.sub sideString (i * sideStringsLength) sideStringsLength
            sideValsArray.[i] <- Array.sub sideValues ((i * sideStringsLength) - 1) (sideStringsLength + 1)
        sideStringsArray.[(horizontalChunks - 1)] <- Array.sub sideString ((horizontalChunks - 1) * sideStringsLength) (sideString.Length - ((horizontalChunks - 1) * sideStringsLength))
        sideValsArray.[(horizontalChunks - 1)] <- Array.sub sideValues (((horizontalChunks - 1) * sideStringsLength) - 1) (sideStringsArray.[(horizontalChunks - 1)].Length + 1)

    use system = ActorSystem.Create "734AssignmentOne"
    let agents = Array.create horizontalChunks (spawn system "actorName" (actorOf (fun msg -> ())))
    // This will probably have a huge performance penalty, since it is initialising actors with a blank function...
    
    let result = System.Threading.Tasks.TaskCompletionSource<int> ()

    let createAgent numInArray = 
        spawn system (sprintf "agentLCS%d" numInArray)
        <| fun inbox ->
            let mutable tStrChunk = [|'a'|]
            let mutable sStrChunk = [|'a'|]
            let mutable tValChunk = [|-1|]
            let mutable sValChunk = [|-1|]
            let mutable returnVal = -1
            let rec loop n = 
                actor {                    
                    
                    let! mess1 = inbox.Receive () // Should only ever be TopVals
                    match mess1 with
                    | TopStr x -> tStrChunk <- x
                    | SideStr x -> sStrChunk <- x
                    | TopVals x -> tValChunk <- x
                    | SideVals x -> sValChunk <- x

                    if n = 0 then
                        sValChunk <- sideValsArray.[numInArray]  // sVal and sStr should never change for each actor
                        sStrChunk <- sideStringsArray.[numInArray]

                    tStrChunk <- topStringsArray.[n]
                    
                    if sValChunk.Length > tValChunk.Length then
                        let (len, rVals, bVals) = findLCSLenSeq sStrChunk tStrChunk sValChunk tValChunk
                        //printfn "%d %d %d" numInArray n len
                        System.Console.WriteLine("{0} {1} {2}", numInArray, n, len)
                        returnVal <- len
                        sValChunk <- rVals
                        if numInArray < (horizontalChunks - 1) then
                            agents.[numInArray + 1] <! (TopVals bVals)

                    else 
                        let (len, bVals, rVals) = findLCSLenSeq tStrChunk sStrChunk tValChunk sValChunk
                        //printfn "%d %d %d" numInArray n len
                        System.Console.WriteLine("{0} {1} {2}", numInArray, n, len)
                        returnVal <- len
                        sValChunk <- rVals
                        if numInArray < (horizontalChunks - 1) then
                            agents.[numInArray + 1] <! (TopVals bVals)

                    if (n = (verticalChunks - 1)) && (numInArray = (horizontalChunks - 1)) then
                        result.SetResult returnVal

                    return! loop (n + 1)
                }
            loop 0

    for i = 0 to (agents.Length - 1) do  // Initialise agents with correct function
        agents.[i] <- createAgent i

    for i = 0 to (verticalChunks - 1) do // Provide the top agent with it's topVals
        agents.[0] <! (TopVals topValsArray.[i])

    let res = result.Task.Result
    res

[<EntryPoint>]
let main argv = 

    try

        let timer = System.Diagnostics.Stopwatch.StartNew()

        let stringOne = System.IO.File.ReadAllText(argv.[0].[4..]).ToCharArray()
        let stringTwo = System.IO.File.ReadAllText(argv.[1].[4..]).ToCharArray()
        let mutable LCSLen = -1
        (*let mode = argv.[2]
        match mode with
            | "/SEQ" -> let (totalLen, bottomValues, rightValues) = if stringOne.Length < stringTwo.Length then findLCSLenSeq stringTwo stringOne (Array.zeroCreate(stringTwo.Length + 1)) (Array.zeroCreate(stringOne.Length + 1))
                                                                    else findLCSLenSeq stringOne stringTwo (Array.zeroCreate(stringOne.Length + 1)) (Array.zeroCreate(stringTwo.Length + 1))
                        LCSLen <- totalLen
            | "/PAR-SYNC" -> LCSLen <- parsync stringOne stringTwo (Array.zeroCreate(stringOne.Length + 1)) (Array.zeroCreate(stringTwo.Length + 1)) 2 2
            | "/PAR-ASYNC" -> LCSLen <- parasync stringOne stringTwo (Array.zeroCreate(stringOne.Length + 1)) (Array.zeroCreate(stringTwo.Length + 1)) 25 25
            | _ -> printfn "Incorrect mode parameter stated"*)

        if argv.[2].Contains("/SEQ") then
            let (totalLen, bottomValues, rightValues) = if stringOne.Length < stringTwo.Length then findLCSLenSeq stringTwo stringOne (Array.zeroCreate(stringTwo.Length + 1)) (Array.zeroCreate(stringOne.Length + 1))
                                                        else findLCSLenSeq stringOne stringTwo (Array.zeroCreate(stringOne.Length + 1)) (Array.zeroCreate(stringTwo.Length + 1))
            LCSLen <- totalLen

        elif argv.[2].Contains("/PAR-SYNC") then        
            let horizontalAgents = System.Int32.Parse(argv.[2].Substring(argv.[2].IndexOf(':') + 1, (argv.[2].IndexOf(',') - argv.[2].IndexOf(':') - 1)))
            let verticalAgents = System.Int32.Parse(argv.[2].Substring(argv.[2].IndexOf(',') + 1))
            LCSLen <- parsync stringOne stringTwo (Array.zeroCreate (stringOne.Length + 1)) (Array.zeroCreate (stringTwo.Length + 1)) verticalAgents horizontalAgents

        elif argv.[2].Contains("/PAR-ASYNC") then
            let horizontalAgents = System.Int32.Parse(argv.[2].Substring(argv.[2].IndexOf(':') + 1, (argv.[2].IndexOf(',') - argv.[2].IndexOf(':') - 1)))
            let verticalAgents = System.Int32.Parse(argv.[2].Substring(argv.[2].IndexOf(',') + 1))
            LCSLen <- parasync stringOne stringTwo (Array.zeroCreate (stringOne.Length + 1)) (Array.zeroCreate (stringTwo.Length + 1)) verticalAgents horizontalAgents

        else
            System.Console.Error.WriteLine("Unknown mode specified.  Program exiting.")

        printfn "%d" LCSLen

        timer.Stop()
        printfn "Total time taken was: %d:%d:%d" timer.Elapsed.Minutes timer.Elapsed.Seconds timer.Elapsed.Milliseconds

        0 // return an integer exit code
    with
        | :? System.IndexOutOfRangeException as err -> System.Console.Error.WriteLine("IndexOutOfRange error occurred in the program's execution.  Error details are: {0}", err);1
        | :? System.SystemException as err -> System.Console.Error.WriteLine("Error occurred in the program's execution.  Error details are: {0}", err); 2