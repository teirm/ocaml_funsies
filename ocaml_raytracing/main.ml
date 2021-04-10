(* A ray tracer in ocaml
   Based on Ray Tracing in One Weekend 
   by Peter Shirley
  @author teirm
*)

(* Going to use the JaneStreet Base and Core Libraries for ease *)
open Base
open Core_kernel

(* types *)
type color = Color of float * float * float 

(* helper functions *)
let write_p3_header ~width ~height file =
    Printf.fprintf file "P3\n%d %d\n255\n" width height 
   
let write_color (Color(r,g,b)) file = 
    let ir = int_of_float (255.999 *. r) in
    let ig = int_of_float (255.999 *. g) in
    let ib = int_of_float (255.990 *. b) in
        Printf.fprintf file "%d %d %d\n" ir ig ib

let generate_colors ~width ~height =
    let y_values = List.init height ~f:(fun y -> height - y - 1) in
    let x_values = List.init width ~f:(fun x -> x) in
    List.map (List.cartesian_product x_values y_values) 
        ~f:(fun (x,y) -> let r = (float_of_int x) /. (float_of_int (width - 1)) in
                         let g = (float_of_int y) /. (float_of_int (width - 1)) in
                         let b = 0.25 in
                         Color(r,g,b))

let () = 
    let width = 256 in
    let height = 256 in
    let colors = generate_colors ~width:width ~height:height in
    let out_file = Out_channel.create "render.ppm" in
        write_p3_header ~width:width ~height:height out_file;
        List.iter colors ~f:(fun color -> write_color color out_file);
        Out_channel.close out_file
