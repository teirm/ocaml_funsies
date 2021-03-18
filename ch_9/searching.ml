(* Searching *)

let rec equal l len p =
    len = 0
  ||
    List.hd l = List.hd p && equal (List.tl l) (len - 1) (List.tl p);;   

let rec search_list_inner p len_p l len_l = 
    len_p <= len_l
    &&
    (equal l len_p p ||
        search_list_inner p len_p (List.tl l) (len_l - 1));;

let rec search_list p l =
    search_list_inner p (List.length p) l (List.length l);;

let swallow_all ch s sp =
    let x = ref sp in
        while !x < String.length s && s.[!x] = ch do x := !x + 1 done;
        !x - sp;;

(*
 * The argument sauce breaks down like this:
 *  p   pattern to find
 *  pp  pattern position -- current position in pattern
 *  s   string to search in 
 *  sp  string position -- current position in string
 *)
let rec at p pp s sp =
    pp > String.length p - 1 ||
    match
        match p.[pp] with
        |   '?' -> 
                if pp + 1 > String.length p - 1 then None           (* end pattern *) 
                else if sp > String.length s - 1 then Some (2, 0)   (* end string *)
                else if p.[pp + 1] = s.[sp] then Some (2, 1)        (* the character *)
                else Some (2, 0)                                    (* any other character *)
        |   '*' ->
                if pp + 1 > String.length p - 1 then None           (* end pattern *)
                else Some(2, swallow_all p.[pp+1] s sp)             (* read zero or more *)
        |   '+' ->
                if pp + 1 > String.length p - 1 then None           (* end pattern *)
                else if sp > String.length s - 1 then None          (* end string *)
                else if p.[pp + 1] = s.[sp] then Some(2, swallow_all p.[pp + 1] s sp)   (* read one or more characters *)
                else None                                           (* did not match *)
        |   c ->
                if sp < String.length s && c = s.[sp] then Some(1,1)
                else None
    with
        None -> false                                                   (* match failure stop *)
    |   Some (jump_p, jump_s) -> at p (pp + jump_p) s (sp + jump_s);;   (* match success -- continue *)


let rec search_string_inner n p s =
    (n < String.length s || n = 0 && String.length s = 0) && 
    (at p 0 s n || search_string_inner (n+1) p s);;

let search_string = search_string_inner 0;;

(* exercises *)
let rec all_matches_inner n pat str accum step =
    if n > (String.length str - String.length pat) then accum 
    else if (at pat 0 str n) then all_matches_inner (n+step) pat str (accum + 1) step
    else all_matches_inner (n+1) pat str accum step ;;

let all_matches p s = all_matches_inner 0 p s 0 1;;

let non_overlapping_matches p s = all_matches_inner 0 p s 0 (String.length p);; 

(* longest suffix *)

(* 
 * Check to see how many characters from p match in s starting 
 * in s from sp
 *)
let check_pattern p s sp =
    let m = ref 0 in
    let x = ref sp in
    while !x < String.length s 
       && !m < String.length p 
       && s.[!x] = p.[!m] do 
        m := !m + 1; x := !x + 1 
    done;
    !m;;

let longest_match p s = 
    let rec longest_match_inner pat str str_pos curr_max curr_pos =
        let new_max = check_pattern pat str str_pos in 
        if (str_pos > String.length str - 1) || curr_max = String.length pat then (curr_max, curr_pos) 
        else if curr_max < new_max then 
            longest_match_inner pat str (str_pos + 1) new_max str_pos 
        else longest_match_inner pat str (str_pos + 1) curr_max curr_pos
    in longest_match_inner p s 0 0 0;;
