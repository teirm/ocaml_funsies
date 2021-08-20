(* 
   Exercises using trees in Ocaml
*)

type 'a tree = 
| Leaf
| Node of 'a * 'a tree * 'a tree

let rec depth = function
| Leaf -> 0
| Node (_, l, r) -> 1 + (max (depth l) (depth r))

let rec shape t1 t2 = 
    match t1, t2 with
    | Leaf, Leaf -> true
    | Leaf, _    -> false
    | _, Leaf    -> false
    | (Node (_, l1, r1)), (Node (_, l2, r2)) -> 
        (shape l1 l2) && (shape r1  r2)

type 'a bst_check =
| Empty
| NotBST
| MinMax of 'a * 'a

let rec inorder = function 
| Leaf -> []
| Node (a, l, r) -> (inorder l) @ [a] @ (inorder r) 

let rec check_monotonic_inc = function
| []        -> true 
| _::[]     -> true
| a::b::xs  -> if a <= b then check_monotonic_inc (b::xs) 
               else false

let is_bst  = function
| Leaf  -> true 
| Node (a, l, r) -> let inordered = inorder (Node (a, l, r)) in
                    if (check_monotonic_inc inordered) then true   
                    else false 
                    
