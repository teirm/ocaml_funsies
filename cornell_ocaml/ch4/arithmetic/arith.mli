module type Arith = sig
    type t
    val zero    : t
    val one     : t
    val (+)     : t -> t -> t
    val ( * )   : t -> t -> t
    val (~-)    : t -> t
end
