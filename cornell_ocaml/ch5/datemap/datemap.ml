(* A map to hold dates *)

type date = { month:int;
              day:int}

module Date = struct 
    type t = date
    let compare t1 t2 = if t1.month > t2.month then 1 
                        else if t1.month < t2.month then -1
                        else if t1.day > t2.day then 1
                        else if t1.day < t2.day then -1
                        else 0
end

module DateMap = Map.Make(Date)

type calendar = string DateMap.t 


let print_calendar cal = 
    DateMap.iter (fun k v -> Printf.printf "(%d, %d) -> %s\n" k.month k.day v) cal 

let () = 
    let myCal = DateMap.(empty |> add {month=1;day=1} "new year"
                               |> add {month=12;day=25} "xmas")
     
    in print_calendar myCal

