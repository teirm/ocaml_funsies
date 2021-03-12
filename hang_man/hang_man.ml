(* User program for hangman game *)

try
    begin 
        let counter = ref 5 in 
        let word    = Hang_man_engine.explode "snakes" in
        let answer  = ref (List.map (fun x -> '_') word) in 
        while !counter > 0 do
            Printf.printf "Enter a character: ";
            let input = read_line () in 
                let res, correct = Hang_man_engine.apply_guess word (input.[0]) in
                    answer := Hang_man_engine.merge_lists !answer res;
                    if Hang_man_engine.solution_complete !answer then
                        begin 
                            Printf.printf "Congrations you won. The word was %s\n" "snakes";
                            counter := !counter - 10
                        end
                    else if correct = 0 then
                        begin
                            counter := !counter - 1;
                            Printf.printf "Incorrect guess. Lost a limb: %d\n" !counter
                        end
                    else
                        begin
                            counter := !counter;
                            Printf.printf "Correct guess %d\n" !counter
                        end
        done
    end
with
    e ->
        Printf.printf "An error occured %s\n" (Printexc.to_string e);
        exit 1

