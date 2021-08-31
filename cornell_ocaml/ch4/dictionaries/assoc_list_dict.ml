module AssocListDict : Dictionary = struct
    type ('k, v') t = ('k * 'v) list

    let empty = []

    let insert k v d = (k,v)::d

    let lookup k d = List.assoc k d
end
