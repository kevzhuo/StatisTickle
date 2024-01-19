module AST

type Distribution = 
|  Unif of int * int
|  Expon of float
|  Norm of int * float
|  FT of float * float
|  Bin of float * int

type Function =
|  Set of Distribution
|  Sample of int
|  Graph of string

type Program = Function List 