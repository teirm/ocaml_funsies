(* 
   Collection of random exercises from chapter 3 that 
   are just put here for no good reason.

   18 August 2021
*)

let rec product = function 
    | []    -> 0
    | x::xs -> x * product xs

let product_fast vals = 
    let rec product_internal lst accum = 
        match lst with 
        | []    -> accum
        | x::xs -> product_internal xs (accum * x)
    in product_internal vals 1

let has_big_red = function 
    | "bigred"::_ -> true
    | _           -> false

let has_two_or_four_elements = function 
    | x::y::[]          -> true
    | x::y::z::q::[]    -> true
    | _                 -> false

let get_fifth_elem lst = 
    if List.length lst < 5 then 0
    else List.nth lst 5

let reverse_sort lst = List.rev (List.sort Stdlib.compare lst)

let my_take lst n =
    let rec my_take_internal l n accum =
        match l,n with 
        | [], _     -> List.rev accum
        | _, 0      -> List.rev accum
        | x::xs, c  -> my_take_internal xs (c-1) (x::accum)
    in my_take_internal lst n []

let rec my_drop lst n = 
    match lst,n with 
    | [], _     -> []
    | xs, 0     -> xs
    | _::xs, c  -> my_drop xs (c-1)

let rec powerset s =  
    let rec merge_and_append n l accum =
        match l with 
        | []    -> accum
        | x::xs -> merge_and_append n xs ((n::x)::x::accum)
    in match s with
       | []     -> [[]]
       | x::xs   -> merge_and_append x (powerset xs) []

let print_int_list lst = 
    List.iter (fun x -> Stdlib.print_int x) lst
