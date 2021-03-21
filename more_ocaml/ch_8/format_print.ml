(* Formatted Printing in SML *)


let rec print_pair_list l = 
    match l with
        []      ->  Printf.printf "\n"
    |   [(x,y)] ->  Printf.printf "(%d, %d)\n" x y
    | (x,y)::tl ->  Printf.printf "(%d, %d) --> " x y; print_pair_list tl;;

let to_hex_string s = 
    let b = Buffer.create (String.length s) in
            for i = 0 to String.length s - 1 do
                Printf.bprintf b "%x" (int_of_char s.[i])
            done;
            Buffer.contents b;;

let rec tabulate_integers w l = 
    match l with 
        []   -> Printf.printf "\n"
    | x::tls -> Printf.printf "(%*d)\n" w x; tabulate_integers w tls;; 

