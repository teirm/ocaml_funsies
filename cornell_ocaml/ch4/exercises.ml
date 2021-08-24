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
