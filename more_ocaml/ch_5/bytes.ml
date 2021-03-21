(* handling bytes in ocaml *)

#use "../ch_4/general_io.ml";;

type input_bits = 
    { input : input;
      mutable byte : int;
      mutable bit  : int};;

let input_bits_of_input i = {
    input = i;
    byte  = 0;
    bit   = 0};;

let rec getbit b = 
    if b.bit = 0 then
        begin 
            b.byte <- int_of_char (b.input.input_char ());
            b.bit  <- 128;
            getbit b
        end
    else 
        let r = b.byte land b.bit > 0 in 
            b.bit <- b.bit / 2;
            r;;

let rec getbyte b = 
    if b.byte = 0 then
        begin
            b.byte <- int_of_char (b.input.input_char ());
            getbyte b
        end
    else 
        let curr_byte = b.byte in
            b.byte <- int_of_char (b.input.input_char ());
            b.bit <- 0;
            curr_byte;;

let align b = b.bit <- 0;;

let getval b n = 
    if n <= 0 || n > (Sys.word_size - 1) then
        raise (Invalid_argument "getval")
    else
        let r = ref 0 in
            for x = n - 1 downto 0 do 
                r := !r lor ((if getbit b then 1 else 0) lsl x)
            done;
            !r;;

(* an optimized version of getval that works for aligned access *)
let getval_optimized b n = 
    if b.bit != 0 
        then raise (Invalid_argument "unaligned input")
    else if n <= 0 || n > (Sys.word_size - 1) || n mod 8 != 0 
        then raise (Invalid_argument "getval_optimized")
    else 
        let r = ref 0 in
            for x = n - 8 downto 0 do
                r := !r lor ((getbyte b) lsl x)
            done;
            !r;;

let getval_32 b n = Int32.of_int (getval b n);;


type output_bits = 
    { output : output;
      mutable obyte : int;
      mutable obit  : int};;

let output_bits_of_output o = 
    { output = o;
      obyte  = 0;
      obit   = 7};;

let flush o = 
    if o.obit < 7 then o.output.output_char (char_of_int o.obyte);
    o.obyte <- 0;
    o.obit  <- 7;;

let rec putbit o b = 
    if o.obit = (-1) then
        begin 
            flush o;
            putbit o b
        end
    else 
        begin
            if b <> 0 then o.obyte <- o.obyte lor (1 lsl o.obit);
            o.obit <- o.obit - 1
        end;;

let rec putbyte o b = flush o;; 

let putval o v l = 
    for x = l - 1 downto 0 do
        putbit o (v land (1 lsl x))
    done

let putval_32 o v l = putval o (Int32.to_int v) l;;

let putval_optimized o v l = 
    if o.obit != 7 
        then raise (Invalid_argument "unaligned output")
    else 
        for x = l - 8 downto 0 do
            putbyte o (v lor (0xff lsl x))
        done

        
