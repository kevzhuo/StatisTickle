module Parser

open Combinator
open AST

let expr, exprImpl = recparser()

let pad p = pbetween pws0 p pws0

let number = 
  pmany1 pdigit |>> (fun ds -> stringify ds |> int)

let intparameter =
  (pright (pstr "-") (number)) |>> (fun x -> -x) <|> number

let floater =
  pseq 
    (intparameter)
    (pright (pchar ('/')) (intparameter))
    (fun (a,b) -> float a / float b)
  <|>
  (intparameter |>> (fun x -> float x))
  

//Uniform (int , int)
let uniform = 
  pright 
    (pstr ("Uniform")) 
    (pseq 
      (pright (pad (pchar ('('))) (pad intparameter))
      (pbetween (pad (pchar (','))) (pad intparameter) (pad (pchar (')'))))
      (fun (first,second) -> Unif (first, second)))

// exponential of float
let exponential = 
  pbetween
    ((pstr ("Exponential")))
    (pright (pad (pchar ('('))) (pad floater))//note this is an int not a float parser
    (pad (pstr (")")))
  |>>  (fun e -> Expon e)    

let F = 
  pright 
    (pstr ("F")) 
    (pseq 
      (pright (pad (pchar ('('))) (pad intparameter))
      (pbetween (pad (pchar (','))) (pad intparameter) (pad (pchar (')'))))
      (fun (first,second) -> FT (first, second)))

// |  Normal of int * float
let normal = 
  pright 
    (pstr ("Normal")) 
    (pseq 
      (pright (pad (pchar ('('))) (pad intparameter))
      (pbetween (pad (pchar (','))) (pad floater) (pad (pchar (')'))))
      (fun (first,second) -> Norm (first, second)))

let binomial = 
  pright 
    (pstr ("Binomial")) 
    (pseq 
      (pright (pad (pchar ('('))) (pad floater))
      (pbetween (pad (pchar (','))) (pad intparameter) (pad (pchar (')'))))
      (fun (first,second) -> Bin (first, second)))


let dist = 
  uniform <|> exponential <|> normal <|> F <|> binomial

let set =
  pbetween
    (pstr ("Set("))
    dist
    (pseq (pstr (")")) (pmany0 (pstr(","))) (fun (a,b) -> a + (System.String.Concat(Array.ofList(b)))))
    //  account for the potential comma with a pseq ^
  |>>  (fun e -> Set e)    

let sample = 
  pbetween
    (pstr ("Sample("))
    intparameter
    //(pstr (")"))
    (pseq (pstr (")")) (pmany0 (pstr(","))) (fun (a,b) -> a + (System.String.Concat(Array.ofList(b)))))
    //  account for the potential comma with a pseq ^
  |>>  (fun e -> Sample e)    


let graph = 
  pbetween
    (pstr ("Graph("))
    (pstr("Distribution") <|> pstr("Density"))
    (pstr (")"))
    //  account for the potential comma with a pseq ^
  |>>  (fun e -> Graph e)    


//func is either a set, sample, or a graph
let func =
  set <|> sample <|> graph

let program = 
  pmany1 (pleft (pad func) (pmany0 (pchar(','))))

exprImpl := program 
let grammar = pleft expr peof

(* PARSER *)
let parse input =
    let i = prepare input
    match grammar i with
    | Success(ast,_) -> Some ast
    | Failure(_,_) -> None