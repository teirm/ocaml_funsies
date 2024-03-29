module type Queue = sig
    (* An ['a queue] is a queue whose elements have type ['a]. *)
    type 'a queue

    (* The empty queue. *)
    val empty : 'a queue

    (* Wether a queue is empty. *)
    val is_empty : 'a queue -> bool

    (* [enqueue x q] is the queue [q] with [x] added to the end. *)
    val enqueue : 'a -> 'a queue -> 'a queue

    (* [peek q] is [Some x], where [x] is the element at the front of the
       queue or [None] if the queue is empty. *)
    val peek : 'a queue -> 'a option

    (* [dequeue q] is [Some q'], where [q'] is the containing all of the
       elements of [q] except the fron of [q], or [None] if [q] is empty. *)
    val dequeue : 'a queue -> 'a queue option
end
