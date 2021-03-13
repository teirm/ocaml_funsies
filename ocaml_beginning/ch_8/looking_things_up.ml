(* Examples and Exercises from ch 8 *)

let first (x, _)  = x;;
let second (_, y) = y;;

let rec lookup key dict = 
    match dict with 
        []          -> raise Not_found
    | (k,v)::xs     -> if k = key then v 
                                  else lookup key xs;;

let rec add key value dict = 
    match dict with 
        []      -> [(key,value)]
    | (k,v)::xs -> if k = key then (k,value)::xs 
                              else add key value xs;;

let rec remove key dict = 
    match dict with 
        []      -> []
    | (k,_)::xs -> if k = key then xs 
                              else remove key xs;;

let key_exists k d = 
    try 
        let _ = lookup k d in true
    with 
        Not_found -> false;;

(* exercises *)

let rec exists key l = 
    match l with 
        []  -> false 
    | x::xs -> if x = key then true else exists key xs;;

let rec unique_keys dict = 
    let rec unique_keys_internal d count seen =
        match d with 
            []          -> count
        | (k,_)::pairs  -> if (exists k seen) = false 
                              then unique_keys_internal pairs (count+1) (k::seen)
                              else unique_keys_internal pairs count seen
    in unique_keys_internal dict 0 [];;

let rec replace key value dict = 
    match dict with 
        []      -> raise Not_found
    | (k,_)::ds -> if k = key then (key,value)::ds else replace key value ds;;

let rec zip l1 l2 = 
    match (l1,l2) with
        [], []      -> []
    |   [], _       -> raise (Invalid_argument "unmatched length")
    |   _, []       -> raise (Invalid_argument "unmatched length")
    | x::xs, y::ys  -> (x,y)::(zip xs ys);;

let rec unzip dict = 
    match dict with 
        []      -> ([], [])
    | (k,v)::ds -> let (keys,values) = unzip ds in (k::keys, v::values);;
                                       
let rec remove_dups l seen =
    match l with
        []      -> []
    | (k,v)::ps -> if exists k seen then remove_dups ps seen
                                    else (k,v)::(remove_dups ps (k::seen))

let rec list_to_dict l = remove_dups l [];;

let rec union d1 d2 = remove_dups (d1 @ d2) [];; 
