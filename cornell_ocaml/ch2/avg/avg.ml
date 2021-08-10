(* Creating an infix operator to take averages *)

let ( +/. ) x y = (x +. y ) /. 2.0

let () = 
    assert ((4.0 +/. 5.0) = 4.5);
    Printf.printf "All tests passed\n"
