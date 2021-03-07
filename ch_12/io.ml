(* input and output in ocaml *)

let print_dict_entry (k,v) =
    print_int k;
    print_newline();
    print_string v;
    print_newline();;

let iter f d = List.map f d 

let print_dict d = iter print_dict_entry d

let rec read_dict () =
    try
        let i = read_int () in
            let name = read_line () in 
                (i, name) :: read_dict ()
    with
        Failure "int_of_string" -> 
            print_string "This is not a valid integer. Try again.";
            print_newline();
            read_dict()
    |   End_of_file -> []

(* File IO *)
let entry_to_channel ch (k,v) = 
    output_string ch (string_of_int k);
    output_char ch '\n';
    output_string ch v;
    output_char ch '\n'

let dictionary_to_channel ch d =
    iter (entry_to_channel ch) d

let dictionary_to_file filename dict = 
    let ch = open_out filename in
        dictionary_to_channel ch dict;
        close_out ch

let entry_of_channel ch = 
    let number = input_line ch in 
    let name   = input_line ch in
        (int_of_string number, name)

let rec dictionary_of_channel ch =
    try 
        let e = entry_of_channel ch in
            e :: dictionary_of_channel ch
    with
        End_of_file -> []

let dictionary_of_file filename = 
    let ch = open_in filename in
        let dict = dictionary_of_channel ch in 
            close_in ch;
            dict

(* exercises *)

let print_int_elem x = print_int x; print_char ';'

let print_list l = 
    let rec print_list_int l =
        match l with
            []  -> ()  
        |   [x] -> print_int x  
        | x::xs -> print_int x; print_char ';'; print_list_int xs
    in
    print_char '[';
    print_list_int l;
    print_char ']'

let rec read_tuple () =
    try 
        let x = read_int () in 
            let y = read_int () in
                let z = read_int () in 
                    (x, y, z)
    with 
        Failure "int_of_string" ->
            print_string "This is not a valid integer. Try again.";
            print_newline();
            read_tuple()

let rec times_table n =
    let rec generate_list n accum = 
        match n with 
            0   -> accum
        |   x   -> generate_list (n-1) (x::accum)
    in let l = generate_list n [] 
    in  let rec times_table_int l1 l2 = 
        match l1 with 
            []  -> []
        |   x::xs -> (List.map (fun v -> v*x) l2)::(times_table_int xs l2)
    in times_table_int l l 

let row_to_channel ch r = 
    List.map (fun x -> output_string ch (string_of_int x); output_char ch '\t') r;
    output_char ch '\n'
   
let table_to_file filename table = 
    let ch = open_out filename in
        List.map (row_to_channel ch) table;
        close_out ch

let line_count filename = 
    let ch = open_in filename in 
        let rec read ch count =
            try 
            let _ = input_line ch in
                read ch (count+1)
            with
                End_of_file -> count
        in read ch 0


let copy_file src dst =
    try 
        let src_ch = open_in src in
            try 
            let dst_ch = open_out dst in
                try 
                    let rec copy_line s_ch d_ch =  
                        let src_line = input_line src_ch
                            in output_string dst_ch src_line;
                               output_char dst_ch '\n';
                               copy_line s_ch d_ch
                    in copy_line src_ch dst_ch
                with 
                    End_of_file -> 
                        close_out dst_ch;
                        close_in src_ch
            with
                Sys_error s -> 
                    print_string "Unable to open destination file: ";
                    print_string s;
                    print_newline ()
    with
        Sys_error s ->
            print_string "Unable to open source file: ";
            print_string s;
            print_newline ()
