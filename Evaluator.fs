module Evaluator
open AST
open System
open Plotly.NET
open FSharp.Stats
open FSharp.Stats.Distributions
// Math.net for sampling and potentially drawing

(* eval
 *   The Probability Distribution interpreter
 *   Takes in as input a Program which is a list of functions
 *   Goes through the different functions within the Program and performs actions based on them
 *   Returns: nothing
 *)

let eval (p: Program) =
    let mutable norm = Continuous.Normal.Init 0. 1
    let mutable exp = Continuous.Exponential.Init 1
    let mutable uniform = Continuous.Uniform.Init 0 1 
    let mutable f = Continuous.F.Init 1 1
    let mutable bin = Discrete.Binomial.Init 0.1 10
    let mutable distr = "norm"
    let mutable low = 0.0
    let mutable high = 0.0
    let mutable bin_high = 0

    let rec evals p  =
        match p with
        | head::tail -> 
            match head with 
            | Set dist ->  
                match dist with
                | Unif (a,b) -> 
                    uniform <- Continuous.Uniform.Init a b
                    distr <- "uni"
                    low <- float(a - (b-a))
                    high <- float(b + (b-a))
                    evals (tail) 
                | Expon a->
                    if a < 0 then 
                        printfn "Please ensure the parameter to the Exponential Distribution is positive"
                        exit 1
                    else  
                    exp <- Continuous.Exponential.Init a
                    distr <- "expo"
                    high <- 10.0
                    evals (tail) 
                | FT (a,b)-> 
                    if a < 0 || b < 0 then 
                        printfn "Please ensure the parameters to the F distribution are positive"
                        exit 1
                    else 
                    f <- Continuous.F.Init a b
                    distr <- "F"
                    low <- float 0
                    high <- float 20
                    evals (tail) 
                | Norm (a,b) -> 
                    norm <- Continuous.Normal.Init a b 
                    distr <- "norm"
                    low <- float a - 5.0 * b
                    high <- float a + 5.0 * b
                    evals (tail)
                | Bin (a,b) -> 
                    if a < 0 || a > 1 then 
                        printfn "Please ensure the first paramter to the Binomal Distribution is a probability between 0 and 1"
                        exit 1
                    else
                    bin <- Discrete.Binomial.Init 0.1 10
                    distr <- "bin"
                    bin_high <- b
                    evals (tail)
            | Sample num -> 
                match distr with
                | "uni" -> 
                    let samples = Seq.init num (fun _ -> uniform.Sample())
                    let sample = String.Join(",", samples)
                    $"You number you have sampled are: {sample}" + evals tail 
                | "expo" -> 
                    let samples = Seq.init num (fun _ -> exp.Sample())
                    let sample = String.Join(",", samples)
                    $"You number you have sampled are: {sample}" + evals tail
                | "norm" -> 
                    let samples = Seq.init num (fun _ -> norm.Sample())
                    let sample = String.Join(",", samples)
                    $"You number you have sampled are: {sample}" + evals tail
                | "F" -> 
                    let samples = Seq.init num (fun _ -> f.Sample())
                    let sample = String.Join(",", samples)
                    $"You number you have sampled are: {sample}" + evals tail
                | "bin" -> 
                    let samples = Seq.init num (fun _ -> bin.Sample())
                    let sample = String.Join(",", samples)
                    $"You number you have sampled are: {sample}" + evals tail
                | _ -> "Never gonna happen"
            | Graph str ->
                match distr with
                | "uni" -> 
                    match str with
                    | "Density" -> 
                        let plotUni =
                            [low..high]
                            |> List.map (fun x -> x,uniform.PDF x)
                            |> Chart.Area
                            |> Chart.withTemplate ChartTemplates.lightMirrored
                            |> Chart.withTitle "Uniform PDF"
                        plotUni |> Chart.show
                        evals tail
                    | "Distribution" -> 
                        let plotUni =
                            [low..high]
                            |> List.map (fun x -> x,uniform.CDF x)
                            |> Chart.Area
                            |> Chart.withTemplate ChartTemplates.lightMirrored
                            |> Chart.withTitle "Uniform CDF"
                        plotUni |> Chart.show
                        evals tail
                    | _ -> "Never going to happen"
                | "expo" -> 
                    match str with
                    | "Density" -> 
                        let plotExp =
                            [low..high]
                            |> List.map (fun x -> x,exp.PDF x)
                            |> Chart.Area
                            |> Chart.withTemplate ChartTemplates.lightMirrored
                            |> Chart.withTitle "Exponential PDF"
                        plotExp |> Chart.show
                        evals tail
                    | "Distribution" -> 
                        let plotExp =
                            [low..high]
                            |> List.map (fun x -> x,exp.CDF x)
                            |> Chart.Area
                            |> Chart.withTemplate ChartTemplates.lightMirrored
                            |> Chart.withTitle "Exponential CDF"
                        plotExp |> Chart.show
                        evals tail
                    | _ -> "Never going to happen"
                | "norm" -> 
                    match str with
                    | "Density" -> 
                        let plotNorm =
                            [low..high]
                            |> List.map (fun x -> x,norm.PDF x)
                            |> Chart.Area
                            |> Chart.withTemplate ChartTemplates.lightMirrored
                            |> Chart.withTitle "Normal PDF"
                        plotNorm |> Chart.show
                        evals tail
                    | "Distribution" -> 
                        let plotNorm =
                            [low..high]
                            |> List.map (fun x -> x,norm.CDF x)
                            |> Chart.Area
                            |> Chart.withTemplate ChartTemplates.lightMirrored
                            |> Chart.withTitle "Normal CDF"
                        plotNorm |> Chart.show
                        evals tail
                    | _ -> "Never going to happen"
                | "F" -> 
                    match str with
                    | "Density" -> 
                        let plotPoisson =
                            [low..high]
                            |> List.map (fun  x -> x,f.PDF x)
                            |> Chart.Area
                            |> Chart.withTemplate ChartTemplates.lightMirrored
                            |> Chart.withTitle "F PDF"
                        plotPoisson |> Chart.show
                        evals tail
                    | "Distribution" -> 
                        let plotPoisson =
                            [low..high]
                            |> List.map (fun x -> x,f.CDF x)
                            |> Chart.Area
                            |> Chart.withTemplate ChartTemplates.lightMirrored
                            |> Chart.withTitle "F CDF"
                        plotPoisson |> Chart.show
                        evals tail
                    | _ -> "Never going to happen"
                | "bin" -> 
                    match str with
                    | "Density" -> 
                        let plotPoisson =
                            [0.. bin_high]
                            |> List.map (fun  x -> x,bin.PMF x)
                            |> Chart.Column
                            |> Chart.withTemplate ChartTemplates.lightMirrored
                            |> Chart.withTitle "Binomial PDF"
                        plotPoisson |> Chart.show
                        evals tail
                    | "Distribution" -> 
                        let plotPoisson =
                            [0.. bin_high]
                            |> List.map (fun x -> x,bin.CDF x)
                            |> Chart.Area
                            |> Chart.withTemplate ChartTemplates.lightMirrored
                            |> Chart.withTitle "Binomial CDF"
                        plotPoisson |> Chart.show
                        evals tail
                    | _ -> "Never going to happen"
                | _ -> "Never gonna happen"
        | [] -> ""
    
    evals p 