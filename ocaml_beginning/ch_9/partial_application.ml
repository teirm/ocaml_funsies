(* Partial Application of functions *)

(* '&' all elements of a list *)
let rec list_and l = 
    let rec list_and_int l accum =
        match l with 
            []  -> accum
        | t::ts -> list_and_int ts (t & accum)
    in list_and_int l true;;

let rec member e l = 
    match l with 
        []      -> false
    | x::xs     -> if x = e then true else member e xs;;

let member_all e l = list_and (List.map (member e) l);;

(* divide all members of a list by 2 *)
let half_members l = List.map (fun x -> x / 2) l;;

(* truncate *)
let rec my_truncate n l = 
    match n, l with
      0, _      -> []
    | _, []      -> []
    | s, x::xs  -> x::(my_truncate (s-1) xs);;

let truncate_ll l n = List.map (my_truncate n) l;;  

let first_ll l safe = List.map (fun x -> match x with 
                                            []  -> safe
                                         | x::_ -> x) l;;
