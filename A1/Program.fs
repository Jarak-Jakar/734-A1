// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open Akka
open Akka.FSharp

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    printfn "Hello world!"
    0 // return an integer exit code
