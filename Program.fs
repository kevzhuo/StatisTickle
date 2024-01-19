open Parser
open Evaluator
open System

let usage() =
  printfn "Usage: dotnet run <function> or dotnet run <filename>"
  printfn "If needed, please read the proper parameters for the different probaility distributions"

[<EntryPoint>]
let main args =
  if args.Length <> 1
  then usage()
  else 
    let asto = parse args.[0]
    match asto with
    | Some ast -> printfn "%A" (eval ast)//fill in with correct evaluate function
    | None -> 
      if (IO.File.Exists (args.[0]))
      then 
        let ast2 = parse (IO.File.ReadAllText (args.[0]))
        match ast2 with 
        | Some ast2 -> printfn "%A" (eval ast2)//fill in with correct evaluate function
        | None -> usage()
      else usage()
  0