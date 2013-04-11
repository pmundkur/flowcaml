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

(* $Id: flowcamlpol.ml,v 1.6 2003/06/30 18:05:33 simonet Exp $ *)

open Format
open Avl_graphics


(***************************************************************************)

type ('a, 'b) interface =
    { int_signature: 'a;
      int_lattice: Principal.lattice;
      int_pci: 'b;
      int_pcf: 'b
    } 

let read_compiled_interface filename =
  let ic = open_in_bin filename in
  try
    let buffer = String.create (String.length Config.magic_int) in
    really_input ic buffer 0 (String.length Config.magic_int);
    if buffer <> Config.magic_int then begin
      close_in ic;
      eprintf "@[<hv>%s@ is not a compiled interface.@]@." filename;
      exit 2
    end;
    let (name, int) = input_value ic in
    int.int_lattice

  with End_of_file | Failure _ ->
    close_in ic;
    eprintf "@[<hv>Corrupted compiled interface@ %s@]" filename;
    exit 2



(***************************************************************************)

let lattice =
  Principal.create ()

let process_file filename =
  let lat = read_compiled_interface filename in
  Principal.merge_into lattice lat



(***************************************************************************)

let graph = ref false
let font = ref ""
let display = ref begin
  try Sys.getenv "DISPLAY"
  with Not_found -> ""
end
let geometry = ref ""



let options = 
  match Config.dot_path with

    None -> 
      [ "-linewidth", Arg.Int (Format.pp_set_margin Format.std_formatter),
	"<int>  Sets the line width to <int>";

      ] 

  | Some p ->

      Avl_graphviz.Dot.set_command p; 

      [ "-display", Arg.String (fun s -> display := s),
	" X display to use for the graphic window";
      
	"-font", Arg.String (fun s -> font := s),
	"<fontname>  Sets the font for graphical output";

	"-linewidth", Arg.Int (Format.pp_set_margin Format.std_formatter),
	"<int>  Sets the line width to <int>";

	"-geometry", Arg.String (fun s -> geometry := s),
	" Sets the geometry of the graphic window";

	"-graph", Arg.Set graph,
	" Enables the graphic window";

      ]



let main () =
  
  Arg.parse options
    process_file

    "usage: flowcamlpol <options> <files>\n Options are:";

  if !graph then begin
    open_graph (Printf.sprintf "%s %s" !display !geometry);
    if !font <> "" then set_font !font;
    ignore (Principal.draw lattice 10 10);
    ignore (Avl_graphics.wait_next_event
              [Avl_graphics.Button_down; Avl_graphics.Key_pressed])
  end
  else fprintf std_formatter "@[%a@]@." Principal.fprint lattice;

  ()


let () =
  try
    main ()
  with
    Graphic_failure msg ->
      eprintf "@[Graphics failure: %s@]@." msg
