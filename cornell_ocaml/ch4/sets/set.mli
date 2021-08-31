module type Set = sig 
    
    type 'a t

    (* [empty] is the empty set *)
    val empty : 'a t

    (* [mem x s] holds iff [x] is an element of [s] *)
    val mem : 'a -> 'a t -> bool

    (* [add x s] is the set [s] unioned with the set
       containing exactly [x] *)
    val add : 'a -> 'a t -> 'a t

    (* [elts s] is a list containing the elements of [s]. No
       guarantee is made about hte odering of the list. *)
    val elts : 'a t -> 'a list
end
