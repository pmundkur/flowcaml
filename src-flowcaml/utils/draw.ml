(**************************************************************************)
(*                                                                        *)
(*                               Flow Caml                                *)
(*                                                                        *)
(*          Vincent Simonet, Projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*  Copyright 2002, 2003 Institut National de Recherche en Informatique   *)
(*  et en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.                  *)
(*                                                                        *)
(*  Author contact: Vincent.Simonet@inria.fr                              *)
(*  Software page: http://cristal.inria.fr/~simonet/soft/flowcaml/        *)
(*                                                                        *)
(**************************************************************************)

(* $Id: draw.ml,v 1.5 2003/06/30 18:05:34 simonet Exp $ *)


(*************************************************************************)
(** Implementations *)

module DrawGraphics = struct

  open Avl_graphics
  
  let draw_lines ~color ~lw = function
      [] -> ()
    | (x0, y0) :: points ->
	set_color color;
	set_line_width lw;
	moveto x0 y0;
	List.iter (function x, y -> lineto x y) points

  let draw_rect ~color ~lw ~x ~y ~w ~h =
    set_color color;
    set_line_width lw;
    draw_rect x y w h

  let draw_ellipse ~color ~lw ~x ~y ~rx ~ry =
    set_color color;
    set_line_width lw;
    draw_ellipse x y rx ry

  let fill_rect ~color ~x ~y ~w ~h =
    set_color color;
    fill_rect x y w h

  let fill_ellipse ~color ~x ~y ~rx ~ry =
    set_color color;
    fill_ellipse x y rx ry

  let fill_poly ~color points =
    set_color color;
    fill_poly (Array.of_list points)

  let draw_text ~color ~x ~y text =
    set_color color;
    moveto x y;
    draw_string text

  let text_size text =
    text_size text

  let draw_vertical_dots ~color ~x ~y ~y' =
    set_color color;
    let y0 = ref y in
    while !y0 <= y' do
      plot x !y0;
      y0 := !y0 + 2
    done

  let draw_horizontal_dots ~color ~x ~x' ~y =
    set_color color;
    let step1 = 3
    and step0 = 4 in
    let x0 = ref x in
    while !x0 <= x' do
      moveto !x0 y;
      lineto (min x' (!x0 + step1)) y;
      x0 := !x0 + step0 + step1
    done

end



module DrawGraphps = struct

  open Graphps
  
  let draw_lines ~color ~lw = function
      [] -> ()
    | (x0, y0) :: points ->
	set_color color;
	set_line_width lw;
	moveto x0 y0;
	List.iter (function x, y -> lineto x y) points

  let draw_rect ~color ~lw ~x ~y ~w ~h =
    set_color color;
    set_line_width lw;
    draw_rect x y w h

  let draw_ellipse ~color ~lw ~x ~y ~rx ~ry =
    set_color color;
    set_line_width lw;
    draw_ellipse x y rx ry

  let fill_rect ~color ~x ~y ~w ~h =
    set_color color;
    fill_rect x y w h

  let fill_ellipse ~color ~x ~y ~rx ~ry =
    set_color color;
    fill_ellipse x y rx ry

  let fill_poly ~color points =
    set_color color;
    fill_poly (Array.of_list points)

  let draw_text ~color ~x ~y text =
    set_color color;
    moveto x y;
    for i = 0 to String.length text - 1 do
      draw_char text.[i]
    done

  let text_size text =
    text_size text

  let draw_vertical_dots ~color ~x ~y ~y' =
    set_color color;
    let y0 = ref y in
    while !y0 <= y' do
      plot x !y0;
      y0 := !y0 + 2
    done

  let draw_horizontal_dots ~color ~x ~x' ~y =
    set_color color;
    let step1 = 3
    and step0 = 4 in
    let x0 = ref x in
    while !x0 <= x' do
      moveto !x0 y;
      lineto (min x' (!x0 + step1)) y;
      x0 := !x0 + step0 + step1
    done

end



(*************************************************************************)
(** Dispatching *)

type window =
    Graphics
  | Graphps

let dispatch f_graphics f_graphps = function
    Graphics -> f_graphics
  | Graphps -> f_graphps

let draw_lines = 
  dispatch DrawGraphics.draw_lines DrawGraphps.draw_lines
let draw_rect = 
  dispatch DrawGraphics.draw_rect DrawGraphps.draw_rect
let draw_ellipse = 
  dispatch DrawGraphics.draw_ellipse DrawGraphps.draw_ellipse
let fill_rect = 
  dispatch DrawGraphics.fill_rect DrawGraphps.fill_rect
let fill_ellipse = 
  dispatch DrawGraphics.fill_ellipse DrawGraphps.fill_ellipse
let fill_poly = 
  dispatch DrawGraphics.fill_poly DrawGraphps.fill_poly
let draw_text = 
  dispatch DrawGraphics.draw_text DrawGraphps.draw_text
let text_size = 
  dispatch DrawGraphics.text_size DrawGraphps.text_size
let draw_vertical_dots = 
  dispatch DrawGraphics.draw_vertical_dots DrawGraphps.draw_vertical_dots
let draw_horizontal_dots =
  dispatch DrawGraphics.draw_horizontal_dots DrawGraphps.draw_horizontal_dots
