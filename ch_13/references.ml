(* references in ocaml *)

let smallest_pow2 x = 
    let t = ref 1 in
        while !t < x do
            t := !t * 2
        done;
        !t

let print_histogram arr =
    print_string "Character frequencies:";
    print_newline ();
    for x = 0 to 255 do
        if arr.(x) > 0 then
            begin
                print_string "For character '";
                print_char (char_of_int x);
                print_string "' (character number ";
                print_int x;
                print_string ") the count is ";
                print_int arr.(x);
                print_string ".";
                print_newline ()
            end
        done

let channel_statistics in_channel = 
    let lines = ref 0 in
    let characters = ref 0 in
    let words = ref 0 in
    let sentences = ref 0 in
    let histogram = Array.make 256 0 in
        try 
            while true do
                let line = input_line in_channel in
                    lines := !lines +1;
                    characters := !characters + String.length line;
                    String.iter 
                        (fun c -> 
                            match c with 
                              '.' | '?' | '!' -> sentences := !sentences + 1
                            | ' ' -> words := !words + 1
                            | _   -> ())
                        line;
                    String.iter 
                        (fun c ->
                            let i = int_of_char c in 
                                histogram.(i) <- histogram.(i) + 1)
                        line
            done
        with
            End_of_file ->
                print_string "There were ";
                print_int !lines;
                print_string " lines, making up ";
                print_int !characters;
                print_string " characters with ";
                print_int !words;
                print_string " words in ";
                print_int !sentences;
                print_string " sentences.";
                print_newline ();
                print_histogram histogram

let file_statistics name = 
    let channel = open_in name in
        try
            channel_statistics channel;
            close_in channel
        with 
            _ -> close_in channel


(* exercises *)

let sum_arr arr =
    let sum = ref 0 in
    for x = 0 to ((Array.length arr) -1) do 
        sum := !sum + arr.(x)
    done;
    !sum

