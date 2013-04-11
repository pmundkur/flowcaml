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

(* $Id: doc_images.ml,v 1.7 2003/06/30 18:05:33 simonet Exp $ *)

open Printf



let convert_cmd = "convert"
let png_output = ref false
let eps_output = ref false


let white = ref Avl_graphics.white



(***************************************************************************)
(** {2 File names} *)

let dirname = ref ""
let subdirname = ref ""
let basename = ref ""
let counter = ref 0

let set_module modulename =
  counter := 0;
  basename := String.uncapitalize modulename

let filename () =
  incr counter;
  sprintf "%s-%04i" (Filename.concat !subdirname !basename
			) !counter

  


(***************************************************************************)


let export_image basename x y w h =

  let oc = open_out (basename ^ ".xpm") in

  let m = Avl_graphics.dump_image (Avl_graphics.get_image x y w h) in

  let t = Hashtbl.create 7 in
  let i = ref 0 in

  let transl color =
    try
      Hashtbl.find t color
    with
      Not_found ->
	incr i;
	Hashtbl.add t color (char_of_int (34 + !i));
	char_of_int (34 + !i)
  in

  let m' = Array.init h (fun i -> String.create w) in

  for i = 0 to h - 1 do
    for j = 0 to w - 1 do
      m'.(i).[j] <- transl m.(i).(j)
    done;
  done;

  fprintf oc "/* XPM */\n";
  fprintf oc "static char *magick[] = {\n";
  fprintf oc "\"%i %i %i %i\",\n" w h !i 1;
  fprintf oc "/* COLORS */\n";
  Hashtbl.iter (fun color char ->
    fprintf oc "\"%c c %s\",\n" 
      char (if color = !white then "None" else sprintf "#%06x" color)
  ) t;
  fprintf oc "/* PIXELS */\n";
  for i = 0 to h - 1 do
    fprintf oc "\"%s\"%s\n" m'.(i) (if i = h - 1 then "" else ",")
  done;
  fprintf oc "\n};\n";

  close_out oc;

  ignore
    (Sys.command (sprintf "%s %s.xpm %s.png" convert_cmd basename basename));
  Sys.remove (sprintf "%s.xpm" basename)



(***************************************************************************)

let is_open = ref false

let open_graph () =
  if !is_open then Avl_graphics.clear_graph () else begin
    Avl_graphics.open_graph " 1024x256";
    white := Avl_graphics.point_color 10 10;
    is_open := true
  end

let value_description vald =

  let basenamerel = filename () in
  let basename = Filename.concat !dirname basenamerel in

  if !png_output then begin
    open_graph ();
    let w, h = Value_description.draw Draw.Graphics vald 0 0 in
    export_image basename 0 0 w (h + 5)
  end;

  if !eps_output then begin
    Graphps.open_eps (sprintf "%s.eps" basename);
    Graphps.open_graph "";
    Graphps.set_text_size 10;
    Graphps.set_font "Courrier";
    let w, h = Value_description.draw Draw.Graphps vald 0 0 in
    Graphps.change_bounding_box w (h + 10);
    Graphps.close_graph ()
  end;

  basenamerel

