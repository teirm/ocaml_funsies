(* set implementations using modules *)

module type SetType = 
    sig 
        type 'a t
        val set_of_list : 'a list -> 'a t
        val list_of_set : 'a t -> 'a list
        val insert      : 'a -> 'a t -> 'a t
        val size        : 'a t -> int
        val member      : 'a -> 'a t -> bool
        val union       : 'a t -> 'a t -> 'a t
    end;;

(* A set as a list *)
module SetList : sig include SetType end =
    struct 
        type 'a t = 'a list;;

        let list_of_set x = x;;

        let insert x l = x::l;;

        let rec set_of_list l =
            match l with [] -> [] | h::t -> insert h (set_of_list t);;
        
        let member = List.mem;;

        let union s1 s2 =
            let rec union_inner s1 s2 accum = 
                match s1 with 
                    [] -> accum
                | x::t -> if member x s2 then union_inner t s2 accum
                          else union_inner t s2 (insert x accum)
            in union_inner s1 s2 s2;;

        let size = List.length;;

    end

let set_list_insert_test arr set =
    let set_r = ref set in 
        for i = 0 to (Array.length arr) - 1 do 
            set_r := SetList.insert arr.(i) !set_r 
        done;
        !set_r;;

(* A set as a binary tree *)
module SetTree : sig include SetType end =
    struct
        type 'a t = Lf | Br of 'a t * 'a * 'a t;;

        let rec list_of_set s = 
            match s with
                Lf -> []
            | Br (l, x, r) -> list_of_set l @ [x] @ list_of_set r;;

        let rec insert x s = 
            match s with
                Lf -> Br (Lf, x, Lf)
            | Br (l, y, r) ->
                if x = y then Br (l, y, r)
                else if x < y then Br (insert x l, y, r)
                else Br (l, y , insert x r);;

        let rec set_of_list l = 
            match l with 
                []  -> Lf
            | h::t  -> insert h (set_of_list t);;
        
        let rec member x s =
            match s with
                Lf -> false
            | Br (_, y, _) when x = y -> true
            | Br (l, y, r) -> if x < y then member x l else member x r;;

        let union s1 s2 = 
            let s1_list = list_of_set s1 in
            let rec union_inner l accum =
                match l with 
                    []  -> accum
                | x::t  -> union_inner t (insert x accum)
            in union_inner s1_list s2;;

        let rec size s = 
            match s with
                Lf -> 0
            | Br (l, _, r) -> 1 + size l + size r;;
        
        end

let binary_tree_insert_test arr set =
    let set_r = ref set in 
        for i = 0 to (Array.length arr) - 1 do 
            set_r := SetTree.insert arr.(i) !set_r 
        done;
        !set_r;;

(* Balanced tree implementation *)
module SetRedBlack : sig include SetType end =
    struct
        type color = R | B;;
        type 'a t = Lf | Br of color * 'a t * 'a  * 'a t;;
        
        let rec list_of_set s =
            match s with 
                Lf -> []
            |   Br (_, l, x, r) -> x :: list_of_set l @ list_of_set r;;
   
        
        (* Hard codes the cases on page 100 *)
        let balance t =
            match t with 
                (B, Br (R, Br (R, a, x, b), y , c), z, d)
            |   (B, Br (R, a, x, Br (R, b, y , c)), z, d)
            |   (B, a, x, Br (R, Br (R, b, y, c), z, d)) 
            |   (B, a, x, Br (R, b, y, Br (R, c, z, d))) ->
                    Br(R, Br(B, a, x, b), y, Br (B, c, z, d))
            |   (a, b, c, d) -> Br (a, b, c, d);;

        let rec insert_inner x s =
            match s with
                Lf -> Br (R, Lf, x, Lf)
            | Br (c, l, y, r) -> 
                if x < y then balance (c, insert_inner x l, y, r)
                else if x > y then balance (c, l, y, insert_inner x r)
                else Br (c, l, y, r);;
        
        let insert x s = 
            match insert_inner x s with
                Br (_, l, y, r) -> Br (B, l, y, r)
            |   Lf -> assert false;; 
        
        let rec set_of_list l =
            match l with 
                [] -> Lf
            | h::t -> insert h (set_of_list t);;
        
        let rec member x s =
            match s with 
                Lf -> false
            | Br (_, l, y, r) ->
                x = y || if x > y then member x r else member x l;;
        
        let union s1 s2 = 
            let s1_list = list_of_set s1 in
            let rec union_inner l accum =
                match l with 
                    []  -> accum
                | x::t  -> union_inner t (insert x accum)
            in union_inner s1_list s2;;
        
        let rec size s = 
            match s with
                Lf -> 0
            | Br (_, l, _, r) -> 1 + size l + size r;;
    end

(* Hash Table implementation *)
module SetHashTbl : sig include SetType end = 
    struct
        type 'a t = ('a, unit) Hashtbl.t ;;

        let list_of_set s = Hashtbl.fold (fun x () l -> x :: l) s [];;
        
        let set_of_list l =
            let s = Hashtbl.create (List.length l) in
                List.iter (fun x -> Hashtbl.add s x ()) l;
                s

        let member x s = Hashtbl.mem s x;;

        let insert x s = if not (member x s) then Hashtbl.add s x ();
                         s;;
        
        let union s1 s2 = 
            let s1_list = list_of_set s1 in
            let rec union_inner l accum =
                match l with 
                    []  -> accum
                | x::t  -> union_inner t (insert x accum)
            in union_inner s1_list s2;;

        let size = Hashtbl.length;;
    end

(* Set module implementation *)
module S = Set.Make (struct type t = int let compare = compare end);;
module SetModule =
    struct  
        type 'a t = S.t;;

        let member x s = S.mem x s;;

        let size s = S.cardinal s;;

        let list_of_set s = S.fold (fun x l -> x::l) s [];;

        let set_of_list l = 
            let s = S.empty in 
                List.fold_right S.add l s;;

        let union s1 s2 = S.union s1 s2;;

        let insert x s = S.add x s;;
    end

(* benchmarking code *)
(* Initialize random module *)
let _ = Random.self_init();;

(* create an array of n unique numbers *)
let generate_test_data ?(scramble=false) n = 
    let a = Array.init n (fun i -> i) in
        if scramble then  begin
            for i = 0 to n-1 do 
                let r_idx = Random.int n in
                let r_val = a.(r_idx) in
                let curr  = a.(i) in
                    a.(i) <- r_val;
                    a.(r_idx) <- curr
            done;
            a;
        end else a;;
  
let print_gc_deltas stat_new stat_old =
    Printf.printf "minor words: %f\npromoted words: %f\nmajor words:%f\n"
        (stat_new.Gc.minor_words -. stat_old.Gc.minor_words)
        (stat_new.promoted_words -. stat_old.promoted_words)
        (stat_new.major_words -. stat_old.major_words);
    Printf.printf "minor collections: %d\nmajor collections: %d\nheap words: %d\nheap chunks: %d\n" 
        (stat_new.minor_collections - stat_old.minor_collections)
        (stat_new.major_collections - stat_old.major_collections)
        (stat_new.heap_words - stat_old.heap_words)
        (stat_new.heap_chunks - stat_old.heap_chunks);;

let time_it f x n =
    let curr_gc_stats = Gc.stat() in
    let t = Sys.time() in
    let _ = f x n in
        Printf.printf "Time: %fs\n" (Sys.time() -. t);
        print_gc_deltas (Gc.stat()) curr_gc_stats;;

let run_set_list_benchmarks n = 
    let s = SetList.set_of_list [] in
    let in_order = generate_test_data n in
    let random = generate_test_data ~scramble:true n in
    time_it set_list_insert_test in_order s;
    time_it set_list_insert_test random s;;

let run_binary_tree_benchmarks n = 
    let s = SetTree.set_of_list [] in
    let in_order = generate_test_data n in
    let random = generate_test_data ~scramble:true n in
    time_it binary_tree_insert_test random s;
    time_it binary_tree_insert_test in_order s;;


