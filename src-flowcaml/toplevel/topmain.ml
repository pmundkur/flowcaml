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

(* $Id: topmain.ml,v 1.6 2003/06/26 13:32:55 simonet Exp $ *)
(* Topmain: the toplevel main program *)



(* *************************************************************************
   Command line arguments
 *)

let options_list = [
  "-I", Arg.String (fun s -> Clflags.include_dirs := s :: !Clflags.include_dirs),
  "<dir>  Add <dir> to the list of include directories";

  "-display", Arg.String (fun s -> Clflags.display := s),
  " X display to use for the graphic window";

  "-font", Arg.String (fun s -> Clflags.font := s),
  "<fontname>  Sets the font for graphical output";

  "-linewidth", Arg.Int (Format.pp_set_margin Format.std_formatter),
  "<int>  Sets the line width to <int>";

  "-geometry", Arg.String (fun s -> Clflags.geometry := s),
  " Sets the geometry of the graphic window";

  "-graph", Arg.Set Clflags.graph,
  " Enables the graphic window";

  "-nopervasives", Arg.Set Clflags.nopervasives,
  " do not open then Pervasives module";

  "-pprint", Arg.String Formatters.parse_pprint,
  "<flags>  Configures pretty print of types.";

  "-debug", Arg.Set Clflags.debug, 
  " print debug informations";

] 

let usage = "Usage: flocaml <options>\nOptions are:"

let anonymous _ = ()



(* *************************************************************************
   Main loop
*)

let main () =
  try
    Arg.parse options_list anonymous usage;
    Toploop.loop Format.std_formatter

  with x ->
    Errors.report_error Format.err_formatter x;
    exit 2


let _ = Printexc.catch main ()
