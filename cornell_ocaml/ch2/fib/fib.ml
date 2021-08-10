(* A program to compute the n-th fibonacci number 
   fast in a reasonable amount of time *)

let fib n = 
    let rec fib_int n a b = 
      if n = 1 then b
      else if n = 2 then b 
      else fib_int (n-1) b (a+b)
    in fib_int n 1 1

let () =
    assert ((fib 5) = 5);
    assert ((fib 16) = 987);
    Printf.printf "All tests passed!\n"
