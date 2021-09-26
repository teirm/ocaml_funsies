(* Implementation of dictionary module type using Tree *)
module type Dictionary = sig
  type ('k, 'v) t

  (* The empty dictionary *)
  val empty  : ('k, 'v) t

  (* [insert k v d] produces a new dictionary [d'] with the same mappings 
   * as [d] and also a mapping from [k] to [v], even if [k] was already 
   * mapped in [d]. *)
  val insert : 'k -> 'v -> ('k,'v) t -> ('k,'v) t

  (* [lookup k d] returns the value associated with [k] in [d].  
   * raises:  [Not_found] if [k] is not mapped to any value in [d]. *)
  val lookup  : 'k -> ('k,'v) t -> 'v
end

module BstDict : Dictionary = struct
    type ('k, 'v) t =  
    | Leaf 
    | Node of 'k * 'v * ('k, 'v) t * ('k, 'v) t

    let empty = Leaf
    
    let rec insert k v dict = 
        match dict with 
        | Leaf -> Node(k, v, Leaf, Leaf)
        | Node(k1, _, left, right) ->
            if k < k1 then insert k v left 
            else if k > k1 then insert k v right
            else Node(k, v, left, right)

    let rec lookup key dict = 
        match dict with 
        | Leaf -> raise Not_found
        | Node (k, v, left, right) ->
            if key < k then lookup key left
            else if key > k then lookup key right
            else v
end
