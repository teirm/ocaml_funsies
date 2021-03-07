(* using the std functions in sml *)

let my_concat l1 l2 = 
    let rec cat_impl l1 l2 accum = 
        match l1, l2 with
            [], [] -> accum
        |   (x::xs), [] -> cat_impl xs [] (x::accum)
        |   _, (y::ys)  -> cat_impl l1 ys (y::accum)
    in cat_impl l1 l2 [];;

let have_true l = List.fold_right ( && ) (List.map (List.mem true) l) true;;

let count_exclaim s = 
    let count = ref 0 in
        String.iter (fun c -> match c with 
                                '!' -> count := !count + 1
                              | _   -> count := !count) s; 
        !count;;

let count_ocaml s = List.fold_right ( fun s accum -> if s = "Ocaml" 
                                                     then accum + 1 
                                                     else accum) (String.split_on_char ' ' s) 0;;


