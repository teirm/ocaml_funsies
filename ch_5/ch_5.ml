(* 
    functions must be declared before use otherwise 
    you get a free variable use error 
*)

(* sort list with nested insertion *)
let rec sort l = 
    let rec insert x l = 
        match l with 
            [] -> [x]
        |   h::t -> if x <= h then x :: h :: t
                              else h :: insert x t 
    in match l with
        [] -> []
    |   h::t -> insert h (sort t);;

(* print a list for reading *)
let rec print_list l = 
    match l with 
        [] -> Printf.printf "\n"
    | x::xs -> Printf.printf "%d; " x; print_list xs;;

(* merge 2 sorted lists into a single sorted list *)
let rec merge x y = 
    match x, y with
        [], l -> l
    |   l, [] -> l
    | hx::tx, hy::ty -> if hx < hy then hx :: merge tx (hy::ty)
                                   else hy :: merge (hx::tx) ty;;

(* drop the first n elements of a list *)
let rec drop n l =
    match l, n with
        [], _   ->  [] 
    |   l,  0   ->  l
    |  x::xs, n ->  drop (n-1) xs;;

(* take the first n elements of a list *)
let rec take n l =
    match l, n with 
        [], _ -> []
    |   l,  0 -> []
    |   x::xs, n -> x :: (take (n-1) xs);;

(* merge sort *)
let rec msort l = 
    match l with
        [] -> []
    |   [x] -> [x]
    |   _   -> let partition = List.length l / 2 in
                let left = take partition l in
                 let right = drop partition l in
                   merge (msort left) (msort right);;

(* check if a list l is sorted *)
let rec is_sorted l = 
    match l with 
        [] -> true
    | a::b::[] -> if a <= b then true else false
    | a::b::xs -> if a <= b then is_sorted (b::xs) else false;;

(* some driver functions *)
let in_list = [53; 9; 2; 6; 19];;
let res = sort in_list;;
let m_res = msort in_list;;

let c_res = sort ['c';'a';'d';'f';'b'];;

print_list res;;
print_list m_res;;
