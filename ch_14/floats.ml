(* floating points in ML *)

let make_vector (x0, y0) (x1, y1) =
    (x1 -. x0, y1 -. y0)

let vector_length (x, y) = 
    sqrt (x *. x +. y *. y)

let offset_point (x,y) (px, py) =
    (px +. x, py +. y)

let scale_to_length l (a, b) =
    let current_length = vector_length (a, b) in
        if current_length = 0. then (a, b) else
            let factor = l /. current_length in 
                (a *. factor, b *. factor)

(* exercises *)
let round_float fp = 
    let ceil_v = ceil fp in
    let floor_v = floor fp in
        if (ceil_v -. fp) > (fp -. floor_v) then floor_v
        else ceil_v

(* equidistance between two points *) 
let equi_distance (x0,y0) (x1,y1) = (((x1 -. x0) /. 2.0), ((y1 -. y0) /. 2.0)) 
   
let speparate fp = 
    let whole = truncate fp in (whole, fp -. (float_of_int whole));;

let star pos =
    let col = truncate (50.0 *. pos) in
    for i = 0 to col do
        print_char ' '
    done;
    print_char '*'

let plot f s e step =
    let curr = ref s in
    while !curr <= e do
        let p = f !curr in
            star p;
            print_newline();
            curr := !curr +. step
    done

