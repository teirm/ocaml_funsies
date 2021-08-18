(* 
   Exercises focusing on types and records
   
   18 Aug 2021
*)

type student = {
    first_name : string;
    last_name  : string;
    gpa        : float;
}

let make_student first last gpa = {first_name=first; last_name=last; gpa=gpa}

let get_name s = (s.first_name, s.last_name)

type poketype = Fire | Water | Normal
type pokemon = {
    name  : string;
    hp    : int;
    ptype : poketype}
let charizard = {name = "Charizard"; hp = 78; ptype = Fire}
let squirtle = {name="Squirtle"; hp = 44; ptype = Water}

let safe_hd = function 
| []    -> None
| x::_  -> Some x

let rec safe_tl = function 
| []    -> None
| x::[] -> Some x
| _::xs -> safe_tl xs 
