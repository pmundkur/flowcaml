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

(* $Id: topgraph.ml,v 1.6 2003/06/30 18:05:33 simonet Exp $ *)

(* Graphical output for the toplevel. *)

open Avl_graphics
open Printf


let vmargin = 10
let hmargin = 10
let scroll_margin = 10
let scroll_line_color = 0xAAAAAA

(***************************************************************************)
(** {2 Opening and closing the graphical window} *)

let state () =
  if !Clflags.graph then begin
    open_graph (sprintf "%s %s" !Clflags.display !Clflags.geometry);
    if !Clflags.font <> "" then set_font !Clflags.font;
    auto_synchronize false
  end 
  else
    close_graph ()



(***************************************************************************)
(** {2 Scrolling} *)

let store : Avl_graphics.image option ref = ref None

let scroll_save () =
  store := Some (get_image 0 0 (size_x ()) (size_y ()))

let scroll_restore y =
  match !store with
    None -> ()
  | Some img ->
      set_color scroll_line_color;
      moveto 0 (y + scroll_margin);
      lineto (size_x ()) (y + scroll_margin);
      draw_image img 0 (y + 2 * scroll_margin)



(***************************************************************************)
(** {2 Drawing items} *)

let draw_flow lat =
  let x, y = Principal.draw lat hmargin vmargin in
  y

let draw_value y string vald =
  set_color black;
  moveto hmargin y;
  draw_string string;
  let x', _ = current_point () in
  let _, y' = Value_description.draw Draw.Graphics vald x' y in
  y'

let draw_signature sg =
  List.fold_left (fun y item ->
    match item with
      Types.Tsig_value (id, vald) ->
	draw_value (y + vmargin) (sprintf "val %s : " (Ident.name id)) vald
    | _ -> y
  ) 0 sg

let draw_eval vald =
  draw_value vmargin "- : " vald



(***************************************************************************)

let mk draw x =
  if !Clflags.graph then begin
    scroll_save ();
    clear_graph ();
    let y = draw x in
    scroll_restore y;
    synchronize ()
  end

let flow = mk draw_flow
let signature = mk draw_signature
let eval = mk draw_eval
