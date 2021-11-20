
(** Context module for handling type checking context *)
module type Context = sig
    (** [t] is the type of a contex. *)
    type t

    (** [empty] is the empty context. *)
    val empty : t

    (** [lookup ctx x] get the binding of [x] in [ctx].
        Raises: [Failure] if [x] is not bound in [ctx]. *)
    val lookup : t -> string -> Ast.typ
    
    (** [extend ctx x ty] is [ctx] extended with a binding 
        of [x] to [ty]. *)
    val extend : t -> string -> Ast.typ -> t
end

module Context : Context = struct
    type t = (string * Ast.typ) list

    let empty = []

    let lookup ctx x = 
        try List.assoc x ctx
        with Not_found -> failwith "Unbound variable"

    let extend ctx x ty = 
        (x, ty) :: ctx
end

open Ast 
open Context

let parse (s : string) : expr = 
    let lexbuf = Lexing.from_string s in 
    let ast = Parser.prog Lexer.read lexbuf
    in ast

(** [typeof env e] s the type of [e] in environment [e].
    That is, it is the [t] such that [env |- e : t]. *)
let rec typeof env = function
  | Bool _ -> TBool
  | Int _ -> TInt
  | Var x -> lookup env x
  | Binop (bop, e1, e2) -> typeof_binop env bop e1 e2
  | Let (x, t, e1, e2) -> typeof_let env x t e1 e2
  | If (e1, e2, e3) -> typeof_if env e1 e2 e3
  | Pair (e1, e2) -> typeof_pair env e1 e2
and typeof_binop env bop e1 e2 =
    match bop, typeof env e1, typeof env e2 with
    | Add, TInt, TInt -> TInt
    | Mult, TInt, TInt -> TInt
    | Leq, TInt, TInt -> TInt
    | _ -> failwith "binop error" 
and typeof_let env x t e1 e2 = 
    let t' = typeof env e1 in 
    if t = t' then
        let env' = extend env x t' in
        typeof env' e2
    else 
        failwith "Let expression type mismatch"
and typeof_if env e1 e2 e3 = 
    let t1 = typeof env e1 in 
    if t1 <> TBool then
        failwith "if guard error"
    else
        let t2 = typeof env e2 in
        let t3 = typeof env e3 in
        if t2 <> t3 then 
            failwith "if branch err"
        else
            t2
and typeof_pair env e1 e2 =
    let t1 = typeof env e1 in
    let t2 = typeof env e2 in
    if t1 <> t2 then
        failwith "pair type err"
    else 
        t1

(** [typecheck e] is [e] if [e] typechecks, that is if there 
    exists a type [t] such that [{} |- e:t]. 
    Raises: [Failure] if [e] does not type check. *)
let typecheck e = ignore (typeof empty e); e

(** [is_value e] is whether [e] is a value. *)
let rec is_value : expr -> bool = function
    | Int _ | Bool _ -> true
    | Pair (e1, e2) -> is_value e1 && is_value e2 
    | Var _ | Let _ | Binop _ | If _ -> false

(** [subst e v x] is [e] with [v] substituted for [x], that
    is [e{v/x}]. *)
let rec subst e v x = match e with 
    | Var y -> if x = y then v else e
    | Bool _ -> e
    | Int _ -> e
    | Binop (bop, e1, e2) -> Binop (bop, subst e1 v x, subst e2 v x)
    | Let (y, t, e1 ,e2) ->
        let e1' = subst e1 v x in 
        if x = y 
        then Let (y, t, e1', e2) (* shadowed name case *)
        else Let (y, t, e1', subst e2 v x)
    | If (e1, e2, e3) ->
      If (subst e1 v x, subst e2 v x, subst e3 v x)
    | Pair (e1, e2) -> Pair (subst e1 v x, subst e2 v x)

(** [step] is the [-->] relation, that is a single step of 
    evaluation. *)
let rec step : expr -> expr = function
    | Int _ | Bool _ -> failwith "Does not step"
    | Var _ -> failwith "Unbound variable"
    | Binop (bop, e1, e2) when is_value e1 && is_value e2 ->
        step_bop bop e1 e2
    | Binop (bop, e1, e2) when is_value e1 ->
        Binop (bop, e1, step e2)
    | Binop (bop, e1, e2) -> Binop (bop, step e1, e2)
    | Let (x, _, e1, e2) when is_value e1 -> subst e2 e1 x
    | Let (x, t, e1, e2) -> Let (x, t, step e1, e2)
    | If (Bool true, e2, _) -> e2
    | If (Bool false, _, e3) -> e3
    | If (Int _, _, _) -> failwith "Guard of if must have type bool"
    | If (e1, e2, e3) -> If (step e1, e2, e3)
    | Pair (e1, e2) when is_value e1 && is_value e2 -> 
        Pair (e1, e2)
    | Pair (e1, e2) when is_value e1 ->
        Pair (e1, step e2)
    | Pair (e1, e2) -> 
        Pair (step e1, step e2)

(** [step_bop bop v1 v2] implements the primitive operation 
    [v1 bop b2]. Requires: [v1] and [v2] are both values. *)
and step_bop bop e1 e2 = match bop, e1, e2 with
    | Add, Int a, Int b -> Int (a + b)
    | Mult, Int a, Int b -> Int (a * b)
    | Leq, Int a, Int b -> Bool (a <= b)
    | _ -> failwith "Operator and operand type mismatch"

(** [eval_small e] is the [e -->* v] relation.  That is
    keep applying [step] until a value is produced. *)
let rec eval_small (e : expr) : expr = 
    if is_value e then e
    else e |> step |> eval_small

(** [eval_big e] is the [e ==> v] relation. *)
let rec eval_big (e : expr) : expr = match e with
    | Int _ | Bool _ -> e
    | Var _ -> failwith "Unbound variable"
    | Binop (bop, e1 ,e2) -> eval_bop bop e1 e2
    | Let (x, _, e1, e2) -> subst e2 (eval_big e1) x |> eval_big
    | If (e1, e2, e3) -> eval_if e1 e2 e3
    | Pair (e1, e2) -> Pair (eval_big e1, eval_big e2)  
    
(** [eval_bop bop e1 e2] if the [e] such that [e1 bop e2 ==> e]. *)
and eval_bop bop e1 e2 = match bop, eval_big e1, eval_big e2 with
    | Add, Int a, Int b -> Int (a + b)
    | Mult, Int a, Int b -> Int (a * b)
    | Leq, Int a, Int b -> Bool (a <= b)
    | _ -> failwith "Operator and operand type mismatch"

(** [eval_if e1 e2 e3] is the [e] such that [if e1 then e2 else e3 ==> e]. *)
and eval_if e1 e2 e3 = match eval_big e1 with
    | Bool true -> eval_big e2
    | Bool false -> eval_big e3
    | _ -> failwith "Guard of if must have type bool"


(** [string_of_val v] converts [v] to a string.
    Requires: [v] represents a value. *)
let string_of_val (e : expr) : string =
    match e with
    | Int i -> string_of_int i
    | Bool b -> string_of_bool b
    | _ -> failwith "precondition violated"


(** [interp s] interprets [s] by lexing and parsing it,
    evaluating it, and converting the result to a string. *)
let interp (s : string) : string = 
    s |> parse |> typecheck |> eval_big |> string_of_val
