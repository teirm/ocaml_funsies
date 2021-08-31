(* 
   exercises from chapter 4 of the 
   cornell ocaml book.
*)

let rec repeat f n x = 
    match n with 
    | 0 -> x
    | a -> repeat f (a-1) (f x)

let product_left lst = List.fold_left ( * ) 1 lst
let product_right lst = List.fold_right ( * ) lst 1

let clip n = 
if n < 0 then 0
else if n > 10 then 10
else n

let cliplist = List.map clip

let exists_fold pred lst = 
    List.fold_left (fun accum v -> (pred v) || accum) false lst

let exists_lib pred lst = 
    if lst = [] then false 
    else List.hd (List.rev (List.sort Bool.compare (List.map pred lst)))  

let budget debit expenses = List.fold_left ( - ) debit expenses

let map_compose f g lst = List.map (fun x -> f (g x)) lst

let longer_than_3 lst = List.filter (fun x -> (String.length x > 3)) lst

let add_1f = List.map (fun x -> x +. 1.0)

let string_delimit sep lst = 
match lst with
| [] -> ""
| x::[] -> x
| x::xs -> x ^  (List.fold_left (fun x y -> x ^ sep ^ y) "" xs) 

type 'a tree =
| Leaf
| Node of 'a * 'a tree * 'a tree

let rec tree_map f t = 
match t with
| Leaf -> Leaf
| Node (v, l, r) -> Node (f v, (tree_map f l), (tree_map f r))

let valid_matrix = function
| [] -> false
| xs -> let row_lengths = List.map List.length xs in 
        let head_length = List.hd row_lengths in
        List.fold_left (fun accum curr -> (curr = head_length) && accum) true row_lengths

let add_row_vectors a_list b_list = List.map2 (fun a b -> a + b) a_list b_list

let matrix_add m1 m2 = List.map2 add_row_vectors m1 m2
