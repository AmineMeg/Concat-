open Sexplib.Std
open Position

type program = expression located list

and expression =
|const of string
|extract pos_expression * pos_expression

and pos_expression =
|forward of int
|backward of int
