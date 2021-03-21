(* Laziness in Ocaml *)

type 'a lazylist = Cons of 'a * (unit -> 'a lazylist);;

let rec lseq n = Cons (n, fun() -> lseq (n+1));;

let lhd (Cons (n, _)) = n;;
let ltl (Cons (_, tf)) = tf ();;

let rec ltake (Cons (h, tf)) n =
    match n with 
        0   -> []
    |   _   -> h :: ltake (tf()) (n-1);;

let rec ldrop (Cons(h, tf) as ll) n = 
    match n with
        0   -> ll
    |   _   -> ldrop (tf()) (n-1);;

let rec lmap f (Cons (h, tf)) = 
    Cons (f h, fun () -> lmap f (tf ()));;

let rec lfilter f (Cons (h, tf)) =
    if f h then
        Cons (h, fun () -> lfilter f (tf ()))
    else 
        lfilter f (tf ());;

let cubes = lfilter (fun x -> x mod 5 = 0) (lmap (fun x -> x*x*x) (lseq 1));;

let rec mkprimes (Cons (h,tf)) = 
    Cons (h, fun () ->
            mkprimes (lfilter (fun x -> x mod h <> 0) (tf ())));;
let primes = mkprimes (lseq 2);;

let rec interleave (Cons (h, tf)) l = 
    Cons (h, fun() -> interleave l (tf()));;

let rec lconst n =
    Cons (n, fun () -> lconst n);;

let interleaved = interleave (lconst 0) (lconst 1);;

let rec allfrom l =
    Cons (l, fun() -> 
                interleave (allfrom (0::l)) (allfrom (1::l)));;

let allones = allfrom [];;

(* exercises *)

let rec pow b n =
    match n with
        0 -> 1
    |   _ -> b * pow b (n-1);;
let rec powers_of_two n = Cons(pow 2 n, fun() -> powers_of_two (n+1));; 

let rec return_elem (Cons(h, tf)) n =
    match n with 
        0   -> h
    |   _   -> return_elem (tf()) (n-1);;

let rec cycle l =
    let rec impl y =
        match y with
            []  -> cycle l
        |x::xs  -> Cons(x, fun () -> impl xs) 
    in impl l;;

let rec mk_fib n m = Cons(n+m, fun () -> mk_fib m (n+m));;
let fibs = mk_fib 0 1;;

let rec double_tl (Cons(h, tf)) = Cons(h, fun () -> double_tl (ltl ((tf()))));;
let unleave (Cons(h, tf) as ll) = let xx = tf() in (double_tl ll, double_tl xx);; 

let rec letter_string n =
    if n <= 26 then
        Char.escaped (char_of_int (n+64))
    else 
        letter_string ((n-1)/26) ^ 
        letter_string (((n-1) mod 26) + 1);;

let alphas = lmap letter_string (lseq 1);;
