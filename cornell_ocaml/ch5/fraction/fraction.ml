module type Fraction = sig
  (* A fraction is a rational number p/q, where q != 0.*)
  type t

  (* [make n d] is n/d. Requires d != 0. *)
  val make : int -> int -> t

  val numerator : t -> int
  val denominator : t -> int
  val to_string : t -> string
  val to_float : t -> float

  val add : t -> t -> t
  val mul : t -> t -> t
end

module MyFraction : Fraction = struct

    type t = Frac of int * int

    let make n d = if d = 0 then raise Division_by_zero
                   else Frac(n, d)

    let numerator (Frac(n, d)) = n
    let denominator (Frac(n, d)) = d
    let to_string (Frac(n, d)) = Printf.sprintf "%d/%d" n d
    let to_float (Frac(n, d)) = Float.of_int n /. Float.of_int d
    
    let add (Frac(n1, d1)) (Frac(n2, d2)) = Frac(n1*d2 + n2*d1, d1*d2)
    let mul (Frac(n1, d1)) (Frac(n2, d2)) = Frac(n1*n2, d1*d2)
end

let () = 
    let f = MyFraction.make 1 2 in 
    Printf.printf "%s\n" (MyFraction.to_string f)
