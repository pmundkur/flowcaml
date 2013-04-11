(**************************************************************************)
(*                                                                        *)
(*                                  Dalton                                *)
(*                      an efficient implementation of                    *)
(*                 type inference with structural subtyping               *)
(*                                                                        *)
(*          Vincent Simonet, Projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*  Copyright 2002, 2003 Institut National de Recherche en Informatique   *)
(*  et en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with the   *)
(*  special exception on linking described in file LICENSE.               *)
(*                                                                        *)
(*  Author contact: Vincent.Simonet@inria.fr                              *)
(*  Software page: http://cristal.inria.fr/~simonet/soft/dalton/          *)
(*                                                                        *)
(**************************************************************************)

(* $Id: dalton_templates.ml,v 1.5 2003/06/30 18:05:32 simonet Exp $ *)

(** Templates of module parameters.

    This unit provides templates of modules which may be used as argument
    for the solver's functor.
  *)

open Format
open Dalton_sig



(*-------------------------------------------------------------------------*)
(** {2 Pretty-print} *)

(** The module [Print] provides a standard style for pretty-printing 
    constraints.
 *)
module Print : PRINT = struct

  open Format

  let ghost = "_"

  let left_destructor printer ppf x =
    fprintf ppf "union(%a)" printer x

  let right_destructor printer ppf x =
    fprintf ppf "inter(%a)" printer x

  let left_destructor_skel printer ppf x =
    fprintf ppf "union~(%a)" printer x

  let right_destructor_skel printer ppf x =
    fprintf ppf "inter~(%a)" printer x

  let rec print_list printer sep ppf = function
      [] -> ()
    | hd :: [] -> printer ppf hd
    | hd :: tl ->
	fprintf ppf "%a%s@ %a"
	  printer hd
	  sep
	  (print_list printer sep) tl

  let same_skel printer ppf list =
    fprintf ppf "@[%a@]" (print_list printer " ~") list

  let equal printer ppf list =
    fprintf ppf "@[%a@]" (print_list printer " =") list

  let leq printer1 printer2 ppf hs1 hs2 =
    fprintf ppf "@[%a < %a@]" 
      printer1 hs1
      printer2 hs2

  let lhs printer ppf list =
    fprintf ppf "@[%a@]" (print_list printer ",") list

  let rhs printer ppf list =
    fprintf ppf "@[%a@]" (print_list printer ",") list

  let first = ref true

  let cset_begin ppf =
    first := true

  let cset_item printer ppf item =
    if !first then begin
      first := false;
      fprintf ppf "@ @[<v>with %a" printer item
    end
    else fprintf ppf "@ and  %a" printer item

  let cset_end ppf =
    if not !first then fprintf ppf "@]"

end



(*-------------------------------------------------------------------------*)
(** {2 Drawing} *)

(** The module [DrawGraphics] provides graphics primitives for the graphics 
    library from the Objective Caml distribution.
 *)
module DrawGraphics : (DRAW with type window = unit) = struct

  type window = unit

  open Avl_graphics
  
  let draw_lines () ~color ~lw = function
      [] -> ()
    | (x0, y0) :: points ->
	set_color color;
	set_line_width lw;
	moveto x0 y0;
	List.iter (function x, y -> lineto x y) points

  let draw_curves () ~color ~lw (x0, y0) points =
    set_color color;
    set_line_width lw;
    moveto x0 y0;
    List.iter (function p0, p1, p2 -> curveto p0 p1 p2) points

  let draw_rect () ~color ~lw ~x ~y ~w ~h =
    set_color color;
    set_line_width lw;
    draw_rect x y w h

  let draw_ellipse () ~color ~lw ~x ~y ~rx ~ry =
    set_color color;
    set_line_width lw;
    draw_ellipse x y rx ry

  let fill_rect () ~color ~x ~y ~w ~h =
    set_color color;
    fill_rect x y w h

  let fill_ellipse () ~color ~x ~y ~rx ~ry =
    set_color color;
    fill_ellipse x y rx ry

  let fill_poly () ~color points =
    set_color color;
    fill_poly (Array.of_list points)

  let draw_text () ~color ~x ~y text =
    set_color color;
    moveto x y;
    draw_string text

  let text_size () text =
    text_size text

  let draw_vertical_dots window ~color ~x ~y ~y' =
    set_color color;
    let y0 = ref y in
    while !y0 <= y' do
      plot x !y0;
      y0 := !y0 + 2
    done

  let draw_horizontal_dots window ~color ~x ~x' ~y =
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


(*-------------------------------------------------------------------------*)
(** {2 Error report} *)

(** The module [ErrorReport] provides standard error report messages.
 *)
module ErrorReport : ERROR_REPORT = struct

  let unification ppf ~term1 ~term2 ~explanation =
    fprintf ppf 
      "@[<v>@[The type@;<1 2>%t@ cannot be unified with@;<1 2>%t@ %t@]@]"
      term1 term2 explanation

  let cycle ppf ~variable ~term =
    fprintf ppf
      "@[<v>@[because the equation@;<1 2>%t = %t@;<1 2>\
      has only recursive solution"
      variable term

  let incompatible ppf ~term1 ~term2 =
    fprintf ppf
      "@[<v>@[because@;<1 2>%t@ is not compatible with@;<1 2>%t@]@]"
      term1 term2

  let minimal ppf ~scheme ~variables =
    fprintf ppf
      "@[<v>@[In the type scheme@;<1 2>%t@ the variable(s)\
      @;<1 2> %t@ has no minimal instance@]@]"
      scheme variables

  let ldestr ppf ~term =
    fprintf ppf
      "@[<v>@[union(.) cannot be applied on the type@;<1 2>%t@]@]" 
      term

  let rdestr ppf ~term =
    fprintf ppf
      "@[<v>@[inter(.) cannot be applied on the type@;<1 2>%t@]@]" 
      term

  let inequality ppf ~explanation =
    fprintf ppf
      "@[<v>@[%t is not valid@]@]" explanation

  let incompatible_schemes ppf ~scheme1 ~scheme2 ~explanation =
    fprintf ppf
      "@[<v>@[The following instance of the provided scheme\
      @;<1 2>%t@ is not compatible with the expected scheme@;<1 2>%t@ %t@]@]"
	scheme1 scheme2 explanation

  let missing_desc ppf ~scheme ~variable ~term =
    fprintf ppf
      "@[<v>@[The expected scheme has the form@;<1 2>%t@ and every \
      instance of the provided one requires@;<1 2>@[<h>%t = %t@]@]@]"
	scheme variable term

  let missing_constraint ppf ~scheme ~constrain =
    fprintf ppf
      "@[<v>@[The expected scheme is equivalent to@;<1 2>%t@ and every \
      instance of the provided one requires@;<1 2>@[<h>%t@]@]@]"
	scheme constrain

  let missing_bound ppf ~scheme ~constrain ~explanation =
    match explanation with
      None ->
	fprintf ppf 
	  "@[<v>@[The expected scheme is equivalent to@;<1 2>%t@ and every \
	  instance of the provided one requires@;<1 2>@[<h>%t@]."
            scheme constrain

    | Some expl ->
	fprintf ppf
	  "@[<v>@[The expected scheme is equivalent to@;<1 2>%t@ and every \
	  instance of the provided one requires@;<1 2>@[<h>%t@]@ \
	  but@;<1 2>@[<h>%t@]@ does not hold."
            scheme constrain expl

end
