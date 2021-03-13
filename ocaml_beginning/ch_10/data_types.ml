(* type constructors *)

type 'a sequence =
    Nil
|   Cons of 'a * 'a sequence;;

let rec length s =
    match s with 
        Nil -> 0
    | Cons (_, t) -> 1+ length t;;

let rec append a b =
    match a with
        Nil -> b
    | Cons (x, t) -> Cons(x, (append t b));;

type expr = 
    Num of int
|   Add of expr * expr
|   Subtract of expr * expr
|   Multiply of expr * expr
|   Divide   of expr * expr
|   Pow      of expr * int;;


let rec pow x n =
    match n with 
        0   -> 1
    |   _   -> x * (pow x (n-1));;

let rec evaluate e =
    match e with 
      Num x         -> x
    | Add (e1, e2)  -> (evaluate e1) + (evaluate e2)
    | Subtract (e1, e2) -> (evaluate e1) - (evaluate e2)
    | Multiply (e1, e2) -> (evaluate e1) * (evaluate e2)
    | Divide   (e1, e2) -> (evaluate e1) / (evaluate e2)
    | Pow      (e1, n)  -> let res = evaluate e1 in pow res n;;

let safe_evaluate e = try Some (evaluate e) with Division_by_zero -> None;;

(* exercises *)
type rect =
    Rectangle of int * int
|   Square    of int;;

let rect_area r =
    match r with 
        Rectangle (l, w)    -> l*w
    |   Square s            -> s*s;;

let rotate r =
    match r with 
        Rectangle (l, w)    -> Rectangle(w, l)
    |   Square s            -> Square s;;

let rec take n s = 
    match n, s with
        0, t    -> Nil
    |   _, Nil  -> raise (Invalid_argument "too short")
    |   m, Cons (x, t) -> Cons (x, take (m-1) t);;

let rec drop n s =
    match n, s with
        0, t    -> t
    |   _, Nil  -> raise (Invalid_argument "too short")
    |   m, Cons (x, t) -> drop (m-1) t;;

let rec map f s =
    match s with
      Nil           -> Nil
    | Cons (x, t)   -> Cons ((f x), map f t);;


