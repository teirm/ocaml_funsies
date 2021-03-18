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

(*
 * The argument sauce breaks down like this:
 *  p   pattern to find
 *  pp  pattern position -- current position in pattern
 *  s   string to search in 
 *  sp  string position -- current position in string
 *  l   length of pattern
 *)
let rec at p pp s sp l =
    l = 0 || p.[pp] = s.[sp] && at p (pp + 1) s (sp + 1) (l - 1);;

let rec search_string_inner n p s =
    String.length p <= String.length s - n
  &&
    (at p 0 s n (String.length p) ||
     search_string_inner (n+1) p s);;

let search_string = search_string_inner 0;;
