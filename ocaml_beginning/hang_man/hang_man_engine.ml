(* This is the main functions that will drive the hang man game *)

(* Did not know you could index into a string like that *)
let explode s = 
    let rec explode_impl s i accum =
        if i < 0 then accum else explode_impl s (i-1) (s.[i]::accum)
    in explode_impl s (String.length s - 1) [];;

(* Apply a guess to the current word *)
let apply_guess word guess =
    let rec apply_guess_impl w res g c =
        match w with 
            []  -> (List.rev res, c > 0)
        | x::xs -> if x = g then apply_guess_impl xs (x::res) g (c+1)
                   else apply_guess_impl xs ('_'::res) g c
    in apply_guess_impl word [] guess 0;;

(* 
 * Need an exception to handle the patterns that 
 * honestly should never occur
 *)
exception Invalid_size

(* 
 * Since guessing returns a new list with only the 
 * guessed blanks filled, we merge the previous state
 * here with the new state...think of it like a redraw
 *)
let merge_lists curr res = 
    let rec merge l1 l2 accum = 
        match l1, l2 with
            [], []      -> List.rev accum
        |   [], _       -> raise Invalid_size
        |   _, []       -> raise Invalid_size
        |  x::xs, y::ys -> if x = y then merge xs ys (x::accum)
                           else if x = '_' && y != '_' then merge xs ys (y::accum)
                           else merge xs ys (x::accum)
    in merge curr res [];;

(*
 * Check if a solution is complete
 *)
let solution_complete soln = List.fold_right (fun x y -> (x != '_') && y) soln true;;
