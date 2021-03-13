(* trees!!! *)

type 'a tree = 
    Br of 'a * 'a tree * 'a tree
|   Lf ;;

let rec size tr =
    match tr with
        Lf  -> 0
    |   Br(_, l, r) -> 1 + size l + size r;;

let rec total tr =
    match tr with 
        Lf  -> 0
    | Br(x, l, r) -> x + total l + total r;;

let rec maxdepth tr = 
    match tr with 
        Lf          -> 0
    | Br(_, l, r)   -> 1 + (fun x y -> if x > y then x else y) (maxdepth l) (maxdepth r);;


let rec list_of_tree tr =
    match tr with
        Lf          -> []
    | Br(x, l, r)   -> (list_of_tree l) @ [x] @ (list_of_tree r);; 

let rec tree_map f tr = 
    match tr with 
        Lf          -> Lf
    | Br(x, l, r)   -> Br(f x, tree_map f l, tree_map f r);;

let rec lookup tr k =
    match tr with
        Lf  -> None 
    | Br((x,v), l, r)   -> if k = x then Some v
                           else if k < x then lookup l k
                           else lookup r k;;

let rec insert tr (key,value) = 
    match tr with
        Lf              -> Br((key, value), Lf, Lf)
    | Br((k,v), l, r)   -> if key = k then Br((k, value), l, r)
                           else if key < k then Br((k, v), (insert l (key, value)), r)
                           else Br((k, v), l, (insert r (key, value)));;

(* exercises *) 
let rec exists tr k =
    match tr with 
        Lf  -> false
    | Br((k',v'), l, r)   -> if k' = k then true
                        else if exists l k then true
                        else exists r k;;

let rec mirror tr = 
    match tr with
        Lf -> Lf 
    | Br(x, l, r) -> Br(x, mirror r, mirror l);;

let rec same_shape tr1 tr2 = 
    match tr1, tr2 with
        Lf, Lf  -> true
    |   _, Lf   -> false
    |   Lf, _   -> false
    | Br(_, l1, r1), Br(_, l2, r2) -> (same_shape l1 l2) && (same_shape r1 r2);;



let rec tree_of_list dict =
    let rec tree_of_list_int d accum = 
        match d with 
            []  -> accum
        | p::ps -> tree_of_list_int ps (insert accum p)
    in tree_of_list_int dict Lf;; 

let rec tree_of_lists d1 d2 = 
    let rec tree_of_lists_int ds accum = 
        match ds with 
            []  -> accum
        | (k,v)::ps -> if exists accum k then tree_of_lists_int ps accum
                       else tree_of_lists_int ps (insert accum (k,v))
    in tree_of_lists_int (d1@d2) Lf;;

type 'a rose_tree = RoseLf | RoseBr of 'a * 'a rose_tree list;;

let rec rose_tree_sum r_tr= 
    match r_tr with
        RoseLf  -> 0
    | RoseBr(x, xs) -> x + (List.fold_left (+) 0 (List.map rose_tree_sum xs));;
    

let rec rose_tree_size r_tr= 
    match r_tr with
        RoseLf  -> 0
    | RoseBr(_, xs) -> 1 + (List.fold_left (+) 0 (List.map rose_tree_size xs));;
