(* permutations *)

#require "more";;
open "More";;

let rec interleave acc e seen l =
    match l with
        []  -> (seen @ [e]) :: acc
    | x::xs -> interleave((seen @ e :: x :: xs) :: acc) e (seen @ [x]) xs;; 

let combine x ps =
    List.concat (List.map (interleave [] x []) ps);;

let rec perms p = 
    match p with
        [] -> [[]]
    | h::t -> combine h (perms t);;

let rec without x l =
    match l with 
        [] -> []
    | h::t -> if h = x then t else h :: without x t;;


(*
 * Algorithm: Take an item for the list 
 *            Produce all permuations of remaining list recursively
 *            Place taken item at head of all produced lists
 *)
let rec perms_2 l = 
    match l with 
        []  -> [[]]
    | l     -> 
        List.concat
            (List.map 
                (fun x -> 
                    List.map (fun l -> x :: l) (perms_2 (without x l))) 
                l);;

(* Dijkstra's Algorithm to generate Lexicographic Permutations *)
let first arr = 
    let f = ref (Array.length arr - 1) in
        for x = 0 to Array.length arr - 2 do
            if arr.(x) < arr.(x+1) then f := x
        done;
        !f;;

let last arr f =
    let c = ref (-1) in
        for x = Array.length arr - 1 downto f + 1 do
            if arr.(x) > arr.(f) && (!c = (-1) || arr.(x) < arr.(!c))
                then c := x
        done;
        !c;;

let swap arr a b = 
    let t = arr.(a) in
        arr.(a) <- arr.(b);
        arr.(b) <- t;;

let sort_subarray arr o l =
    let sub = Array.sub arr o l in
        Array.sort compare sub;
        Array.blit sub 0 arr o l;;

let next_permutation arr_in =
    let arr = Array.copy arr_in in
    let f = first arr in
    let c = last arr f in
        swap arr f c;
        sort_subarray arr (f+1) (Array.length arr - 1 - f);
        arr;;

let non_increasing arr = 
    Array.length arr <= 1 ||
    let r = ref true in
        for x = 0 to Array.length arr - 2 do
            if arr.(x+1) > arr.(x) then r := false
        done;
        !r;;

let all_permutations arr = 
    let copy = Array.copy arr in
        Array.sort compare copy;
        let perm = ref copy in
        let perms = ref [copy] in
            while not (non_increasing !perm) do 
                perm := next_permutation !perm;
                perms := !perm :: !perms;
            done;
            Array.of_list (List.rev !perms);;

(* Exercises *)

let rec combinations l = 
    match l with 
        []     -> [[]]
    | x::tl    -> let tl_combos = combinations tl in 
                    (List.map (fun y -> x::y) tl_combos) @ (tl_combos);;

let permicombinations l = List.concat (List.map (fun l -> perms_2 l) (combinations l));;

let rec tf_lists n accum= 
    match n with
        0   -> [] :: accum
    |   _   -> (List.map (fun l -> true::l) (tf_lists (n-1) accum)) @ 
               (List.map (fun l -> false::l) (tf_lists (n-1) accum));;  
