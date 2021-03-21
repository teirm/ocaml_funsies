(* labelled arguments in ocaml *)

let fill a ~start ~length v = 
    for x = start to start + length - 1 do a.(x) <- v done;;

let divide ~x ~y = x / y;;

(*
let rec split ?(chunksize = 1) l =
    try
        Util.take l chunksize ::
        split ~chunksize (Util.drop l chunksize)
    with
        _ -> match l with [] -> [] | _ -> [l];;
*)

let makeArray ~size ~elem = 
    Array.make size elem;;

type start = Start of int;;
type length = Length of int;;

let fill a (Start s) (Length l) v =
    for x = s to s + l - 1 do a.(x) <- v done;;

let rec map ?(accum = []) f l = 
    match l with
        []  -> List.rev accum 
    | h::t  -> map ~accum:(f h :: accum) f t;; 
