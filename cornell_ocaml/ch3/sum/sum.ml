(* A very basic summation function to learn ounit *)

let rec sum = function 
| []    -> 0
| x::xs -> x + sum xs
