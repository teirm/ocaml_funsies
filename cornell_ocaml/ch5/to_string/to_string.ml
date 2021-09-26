(* Writing functors that support to_string *)

module type ToString = sig
    type t
    val to_string : t -> string
end

module Print (M : ToString) = struct
    let print t = M.to_string t
end


module Int : (ToString with type t = int) = struct 
    type t = int
    let to_string t = Printf.sprintf "%d" t
end

module MyString : (ToString with type t = string) = struct
    type t = string
    let to_string t = t
end

module PrintInt = Print(Int)
module PrintString = Print(MyString)

module StringWithPrint = struct
    include Print(MyString)
    include String
end


let () = 
    Printf.printf "%s\n" (PrintInt.print 10);
    Printf.printf "%s\n" (PrintString.print "dogfish")
