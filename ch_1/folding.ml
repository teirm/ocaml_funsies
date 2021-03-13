(* Exercises and Examples from ch 1 *)

type 'a tree =
    Lf
|   Br of 'a * 'a tree * 'a tree

let rec fold_tree f e t = 
    match t with
        Lf -> e
    | Br (x, l, r) -> f x (fold_tree f e l) (fold_tree f e r);;

let tree_preorder t     = fold_tree (fun x l r -> [x] @ l @ r) [] t;;
let tree_inorder t      = fold_tree (fun x l r -> l @ [x] @ r) [] t;;
let tree_postorder t    = fold_tree (fun x l r -> l @ r @ [x]) [] t;;

(* exercises *)
let accounting expenses budget = List.fold_left (fun x y -> x - y) budget expenses;;

let list_len l = List.fold_left (fun s _ -> s+1) 0 l;;

let get_last l = List.hd (List.fold_right (fun x r  -> match r with 
                                                         [] -> [x] 
                                                       | _  -> r) l []);;

let rev_list l = List.fold_left (fun a x ->  x::a) [] l;;

let member v l = List.fold_left (fun a x -> if x=v then a || true else a) false l;; 

let join_strings l = List.fold_left (fun a s -> s ^ " " ^ a) "" l;;
