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

type date = int * int * int

let is_before date_1 date_2 =
    let (y1,m1,d1) = date_1 and (y2,m2,d2) = date_2 in
        if y1 < y2 then true 
        else if m1 < m2 then true
        else if d1 < d2 then true
        else false

let date_compare d1 d2 = 
    if is_before d1 d2 then -1 
    else if is_before d2 d1 then  1 
    else 0

let earliest = function 
| [] -> None
| xs -> Some (List.hd (List.rev (List.sort date_compare xs))) 

type suit = Clubs | Diamonds | Hearts | Aces
type rank = int
type card = {suit:suit; rank:rank}
