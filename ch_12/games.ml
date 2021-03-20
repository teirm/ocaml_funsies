(* tic-tac-toe *)

type turn = O | X | E;;

let won [a;b;c;d;e;f;g;h;i] =
    a && b && c || d && e && f || g && h && i || a && d && g ||
    b && e && h || c && f && i || a && e && i || c && e && g;;

let empty b = 
    List.map snd 
        (List.filter (fun (t,_) -> t = E)
         (List.combine b [1;2;3;4;5;6;7;8;9]));;

let replace turn board p =
    Util.take board (p-1) @ [turn] @ Util.drop board p;;

let flip_turn t = 
    match t with O -> X | X -> O;;

type tree = Move of turn list * tree list;;
let rec next_moves turn board =
    let next = 
        if won (List.map (( = ) O) board) ||
           won (List.map (( = ) X) board) 
        then
            []
        else 
            List.map 
                (next_moves (flip_turn turn))
                (List.map (replace turn board) (empty board))
    in 
        Move (board, next);;

let rec num_wins turn (Move (b, bs)) = 
    (if won (List.map (( = ) turn) b) then 1 else 0) + 
        List.fold_left ( + ) 0 (List.map (num_wins turn) bs);;

let rec num_draws (Move (b, bs)) = 
    (if (empty b = [] && 
        (won (List.map (( = ) X) b) = false) &&
        (won (List.map (( = ) O) b) = false)) then 1 else 0) +
        List.fold_left ( + ) 0 (List.map num_draws bs);;

let check_won turn b =
    List.fold_left ( + ) 0 
        (List.map snd 
            (List.filter (fun (t, _) -> t = turn) 
                (List.combine b [8;1;6;3;5;7;4;9;2])));;
