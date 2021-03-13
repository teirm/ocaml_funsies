(* User program for hangman game *)

let process_answer ans correct =
    let complete = Hang_man_engine.solution_complete ans in
    match complete, correct with
        true, _      -> Printf.printf "Victory\n"; 1 
    |   false, true  -> Printf.printf "Correct guess\n"; -0  
    |   false, false -> Printf.printf "Incorrect guess\n"; -1;;
    
       
let rec pretty_print_list l = 
    match l with 
        []  -> Printf.printf "\n"
    | x::xs -> Printf.printf "%c " x; pretty_print_list xs;;

try
    begin 
        let counter = ref 5 in 
        let word    = Hang_man_engine.explode "snakes" in
        let answer  = ref (List.map (fun x -> '_') word) in 
        while !counter > 0 do
            Printf.printf "Guesses: ";
            pretty_print_list !answer;
            Printf.printf "Remaining Limbs: %d\n" !counter;
            Printf.printf "Enter a character: ";
            let input = read_line () in 
                let res, correct = Hang_man_engine.apply_guess word (input.[0]) in
                    answer := Hang_man_engine.merge_lists !answer res;
                    let update = process_answer !answer correct in
                    if update = 1 then exit 1
                    else if update = -1 then (counter := !counter + update; Printf.printf "Lost a limb\n"; )
                    else counter := !counter
        done;
        Printf.printf "You died. The word was: ";
        pretty_print_list word
    end
with
    e ->
        Printf.printf "An error occured %s\n" (Printexc.to_string e);
        exit 1

