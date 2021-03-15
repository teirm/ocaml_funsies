(* Compression in Ocaml *)

#use "../ch_4/general_io.ml";;

let string_of_int_list l =
    let b = Bytes.create (List.length l) in
        List.iteri (fun n x -> Bytes.set b n (char_of_int x)) l;
        Bytes.to_string b;;

let int_list_of_string s =
    let l = ref [] in
        for x = String.length s - 1 downto 0 do 
            l := int_of_char s.[x] :: !l
        done;
        !l;;

let output_of_buffer b = 
    {output_char = Buffer.add_char b;
     out_channel_length = fun () -> Buffer.length b};;

let process f s = 
    let b = Buffer.create (String.length s) in 
        f (input_of_string s) (output_of_buffer b);
        Buffer.contents b;;

(* End-of-Data exception *)
exception EOD;;

let decompress i o =
    try
        while true do
            match int_of_char (i.input_char ()) with
                x when x >= 0 && x <= 127 -> 
                    for p = 1 to x + 1 do o.output_char (i.input_char ()) done
            |   x when x > 128 && x <= 255 ->
                    let c = i.input_char () in
                        for p = 1 to 257 - x do o.output_char c done
            |   _ -> raise EOD
        done
    with 
        EOD -> ();;

let decompress_string = process decompress;;

let get_same i = 
    let rec getcount ch c =
        if c = 128 then 128 else
            try 
                if i.input_char () = ch
                    then getcount ch (c+1)
                    else (rewind i; c)
            with
                End_of_file -> c
    in
        let ch = i.input_char () in (ch, getcount ch 1);;

let get_different i = 
    let rec getdiffinner a c =
        if c = 128 then List.rev a
        else
            try 
                let ch' = i.input_char () in
                    if ch' <> List.hd a
                        then getdiffinner (ch' :: a) (c + 1)
                    else (rewind i; rewind i; List.rev (List.tl a))
            with
                End_of_file -> List.rev a
    in getdiffinner [i.input_char ()] 1;;

let compress i o = 
    try
        while true do
            match get_same i with 
                (_, 1) ->
                    rewind i;
                    let cs = get_different i in
                        o.output_char (char_of_int (List.length cs - 1));
                        List.iter o.output_char cs
            |   (b, c) ->
                    o.output_char (char_of_int (257 - c));
                    o.output_char b
        done
    with
        End_of_file -> o.output_char (char_of_int 128);;

let compress_string = process compress;
