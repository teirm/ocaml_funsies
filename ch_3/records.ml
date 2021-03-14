(* records in ml *)

#load "unix.cma";;

type 'a point = {x : float; y : float; label : string; content : 'a};;

let make_point x y l c = 
    {x = x; y = y; label = l; content = c};;

let string_of_point p = 
    p.label ^ " = (" ^ string_of_float p.x ^ ", " ^ string_of_float p.y ^ ")";;

let relabel p label = { p with label};;

let mirror p = {p with x = p.y; y = p.x};;

(* exercises *)
let update_ref r a = r.contents <- a;;


let time_str = 
    let get_time_str curr_time = 
        let tr = Unix.gmtime (curr_time) in 
           Printf.sprintf "%d-%d-%d %d:%d:%d"
            (tr.tm_year+1900) tr.tm_mon tr.tm_mday tr.tm_hour tr.tm_min tr.tm_sec
    in get_time_str (Unix.time());;

Gc.set { (Gc.get()) with Gc.verbose = 0x00e };; 
Gc.print_stat stdout;;
