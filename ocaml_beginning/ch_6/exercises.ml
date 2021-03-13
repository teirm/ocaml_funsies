(* examples from chapter 6 *)

let rec double l = 
    match l with
        []  -> []
    | x::xs -> (x*2) :: double xs;;

let rec evens l =
    match l with 
        []  -> []
    | x::xs -> (x mod 2 = 0) :: evens xs;;

let rec map func l =
    match l with 
        []  -> []
    | x::xs -> func x :: map func xs;;

(* using anonymous function notation *)
map (fun x -> x/2) [10;20;30];;

(* exercises *)

(* replace '!' with '.' in a list *)

let replace_exclaim = map (fun x -> match x with 
                                        '!' -> '.'
                                    |   _   ->  x);;

let clip x = if x > 10 then 10 else if x < 1 then 1 else x;;

let clip_list = map clip;;

let rec apply f n x =
    match n with 
        0 -> x
    |   _ -> f (apply f (n-1) x);;

let rec filter test l = 
    let rec filter_int test l accum =
        match l with 
            []  -> accum
        | x::xs -> if test x then filter_int test xs (x::accum)
                             else filter_int test xs accum
    in filter_int test l [];;

let rec for_all test l = 
    match l with 
         [] -> true
    | x::xs -> if test x then for_all test xs 
                         else false;;

let rec mapl func l = 
    match l with 
        []  -> []
    | x::xs -> (map func x) :: (mapl func xs);; 
