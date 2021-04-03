(* Smaller refresher on Types in Ocaml *)

type pair_of_ints = { a : int; b: int};;

type foo = Nothing | Int of int | Pair of int * int | String of string;;

type expr = 
    | Plus of expr * expr 
    | Minus of expr * expr
    | Times of expr * expr
    | Divide of expr * expr
    | Value of string;;

let rec to_string e = 
    match e with 
    | Plus (left, right) -> 
        Printf.sprintf "(%s + %s)" (to_string left) (to_string right)
    | Minus (left, right) -> 
        Printf.sprintf "(%s - %s)" (to_string left) (to_string right)
    | Times (left, right) -> 
        Printf.sprintf "(%s * %s)" (to_string left) (to_string right)
    | Divide (left, right) ->
        Printf.sprintf "(%s / %s)" (to_string left) (to_string right)
    | Value (v) -> v;;
