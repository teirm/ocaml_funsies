(* examples and exercises *)

let rec take n l = 
    match l with 
        []  -> if n = 0 then [] else raise (Invalid_argument "take")
    | h::t  -> if n < 0 then raise (Invalid_argument "take") else 
                    if n = 0 then [] else h :: take (n - 1) t;;

let rec drop n l = 
    match l with 
        []  -> if n = 0 then [] else raise (Invalid_argument "drop")
    | h::t  -> if n < 0 then raise (Invalid_argument "drop") else 
                    if n = 0 then l else drop (n - 1) t;;

let safe_divide x y = 
    try x / y with 
        Division_by_zero -> 0;;

let rec last l = 
    match l with 
        []  -> raise (Not_found)
    |  [x]  -> x
    | _::t  -> last t;;

let rec smallest l = 
    let rec smallest_int l curr_sm =
        match l with
            []  -> if curr_sm < 0 then raise (Not_found) else curr_sm
        | x::xs -> if (curr_sm < 0 && x > curr_sm) || 
                      (curr_sm >= 0 && x >= 0 && x < curr_sm) then smallest_int xs x
                   else smallest_int xs curr_sm
    in smallest_int l (-1);;

let smallest_or_zero l = try smallest l with 
                            Not_found -> 0;;

exception Negative_sqrt of float;;

let floor_sqrt n = if n < 0.0 then raise (Negative_sqrt n)
                   else floor (sqrt n);;

let floor_sqrt_safe n = try floor_sqrt n with (Negative_sqrt n) -> 0.0;;
