(* find instances of a string in a file *)

let search_from_channel in_channel s = 
    let lines = ref [] in
    let line_number = ref 1 in
    try 
        while true do 
            let line = input_line in_channel in
                if (List.mem s (String.split_on_char ' ' line)) 
                then
                    begin 
                        lines := (line, !line_number)::(!lines); 
                        line_number := !line_number + 1
                    end 
                else
                    
                    line_number := !line_number + 1
        done;
        []
    with
        End_of_file -> !lines

let rec print_results results =
    match results with 
        []      -> Printf.printf "\n" 
    | [(s,n)]   -> Printf.printf "%d: %s\n" n s
    | (s,n)::rs -> print_results rs; Printf.printf "%d: %s\n" n s;;

let search_from_file filename s =
    let channel = open_in filename in
    let results = search_from_channel channel s in
        close_in channel;
        print_results results;;


(* main *)
try 
    begin match Sys.argv with
        [|_; filename; term|] -> search_from_file filename term
    |   _                     -> Printf.printf "Usage: my_search <filename> <term>\n";
    end
with
    e ->
        Printf.printf "An error occured %s\n" (Printexc.to_string e);
        exit 1
