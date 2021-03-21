module type Stack = sig
    type 'a stack
    val empty : 'a stack
    val is_empty : 'a stack -> bool
    val push     : 'a -> 'a stack -> 'a stack
    val peek     : 'a stack -> 'a
    val pop      : 'a stack -> 'a stack
    val format   : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a stack -> unit
end

module ListStack : Stack = struct
    type 'a stack = 'a list
    let empty = []
    let is_empty s = (s = [])

    let push x s = x :: s

    let peek = function
        | [] -> failwith "Empty"
        | x::_ -> x

    let pop = function  
        | [] -> failwith "Empty"
        | _::xs -> xs

    let format fmt_elt fmt s =
        Format.fprintf fmt "[";
        List.iter (fun elt -> Format.fprintf fmt "%a; " fmt_elt elt) s;
        Format.fprintf fmt "]"
    end

module type X = sig
    val x : int
end

module IncX (M: X) = struct 
    let x = M.x + 1 
end

module StackTester (S:Stack) = struct
    assert (S.(empty |> push 1 |> peek) = 1)
end
