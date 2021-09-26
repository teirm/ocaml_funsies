(* Implementations of Queue signature taken from Cornell book *)
module type Queue = sig
  (* An ['a queue] is a queue whose elements have type ['a]. *)
  type 'a queue

  (* The empty queue. *)
  val empty : 'a queue

  (* Whether a queue is empty. *)
  val is_empty : 'a queue -> bool

  (* [enqueue x q] is the queue [q] with [x] added to the end. *)
  val enqueue : 'a -> 'a queue -> 'a queue

  (* [peek q] is [Some x], where [x] is the element at the front of the queue,
     or [None] if the queue is empty. *)
  val peek : 'a queue -> 'a option

  (* [dequeue q] is [Some q'], where [q'] is the queue containing all the elements
     of [q] except the front of [q], or [None] if [q] is empty. *)
  val dequeue : 'a queue -> 'a queue option
end

module TwoListQueue : Queue = struct
  (* [{front=[a;b]; back=[e;d;c]}] represents the queue
     containing the elements a,b,c,d,e. That is, the
     back of the queue is stored in reverse order.
     [{front; back}] is in *normal form* if [front]
     being empty implies [back] is also empty.
     All queues passed into or out of the module
     must be in normal form. *)
  type 'a queue = {front:'a list; back:'a list}

  let empty = {front=[]; back=[]}

  let is_empty = function
    | {front=[]; back=[]} -> true
    | _ -> false

  (* Helper function to ensure that a queue is in normal form. *)
  let norm = function
    | {front=[]; back} -> {front=List.rev back; back=[]}
    | q -> q

  let enqueue x q = norm {q with back=x::q.back}

  let peek = function
    | {front=[]; _} -> None
    | {front=x::_; _} -> Some x

  let dequeue = function
    | {front=[]; _} -> None
    | {front=_::xs; back} -> Some (norm {front=xs; back})
end

module ListQueue : Queue = struct
  (* Represent a queue as a list.  The list [x1; x2; ...; xn] represents
     the queue with [x1] at its front, followed by [x2], ..., followed
     by [xn]. *)
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


(* Creates a ListQueue filled with [n] elements. *)
let fill_listqueue n =
  let rec loop n q =
    if n=0 then q
    else loop (n-1) (ListQueue.enqueue n q) in
  loop n ListQueue.empty

(* Creates a TwoListQueue filled with [n] elements. *)
let fill_twolistqueue n = 
    let rec loop n q = 
        if n = 0 then q
        else loop (n-1) (TwoListQueue.enqueue n q) in
    loop n TwoListQueue.empty

let () = 
    Printf.printf "Running fill_listqueue:\n";
    let t = Sys.time() in
    let n = 1000000000 in 
    let _ = fill_twolistqueue n in 
    Printf.printf "%d elements time: %f\n" n (Sys.time() -. t);
