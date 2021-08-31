module ListSetNoDups : Set = struct
    type 'a t   = 'a list
    let empty   = []
    let mem     = List.mem
    let add x s = if mem x s then s else x::s
    let elts s = s 
end
