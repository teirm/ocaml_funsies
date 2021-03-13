(* Interface for hang man engine *)
exception Invalid_size
val explode: string -> char list
val apply_guess: char list -> char -> (char list * bool)
val merge_lists: char list -> char list -> char list
val solution_complete: char list -> bool
