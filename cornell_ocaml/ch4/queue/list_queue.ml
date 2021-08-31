module ListQueue : Queue = struct
    (* Represent a queue as a list. The list [x1; x2; ...; xn] 
       represents the queue with [x1] at its front, followed by
       [x2], ..., followed by [xn]. *)
    type 'a queue = 'a list

    let empty = []

    let is_empty q = q = []

    let enqueue x q = q @ [x]

    let peek = function
      | [] -> None
      | x::_ -> Some x

    let dequeue = function
      | [] -> None
      | _::q -> Some q
end
