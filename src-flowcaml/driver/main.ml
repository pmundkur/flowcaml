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

(* $Id: main.ml,v 1.9 2003/06/26 13:32:49 simonet Exp $ *)
(* Main: driving flocamlc *)



(* *************************************************************************
   Processing files
 *)

let process_file ppf name =
  if Filename.check_suffix name Config.ext_pimp then
    Compile.implementation ppf name
  else if Filename.check_suffix name Config.ext_pint then
    Compile.interface ppf name
  else
    raise(Arg.Bad("don't know what to do with " ^ name))



let print_version_and_libraries () =
  print_string "The Flow Caml compiler, version ";
  print_endline Config.version;
  print_string "Standard library directory: ";
  print_endline Config.standard_library; 
  print_string "Run-time library directory: ";
  print_endline Config.run_library; 
  exit 0

let print_version () =
  print_endline Config.version;
  exit 0

let print_standard_library () =
  print_string Config.standard_library; 
  print_newline(); 
  exit 0

let print_run_library () =
  print_string Config.run_library;
  print_newline();
  exit 0


(* *************************************************************************
   Command line arguments
 *)

let options_list = [

  "-display", Arg.String (fun s -> Clflags.display := s),
  " X display to use for the graphic window";

  "-dump", Arg.Set Clflags.dumped_output,
  " Dumped output";

  "-font", Arg.String (fun s -> Clflags.font := s),
  "<fontname>  Sets the font for graphical output";

  "-geometry", Arg.String (fun s -> Clflags.geometry := s),
  " Sets the geometry of the graphic window";

  "-graph", Arg.Set Clflags.graph,
  " Enables the graphic window";

  "-i", Arg.Set Clflags.print_types , 
  " Print the interface";

  "-I", Arg.String (fun s -> Clflags.include_dirs := s :: !Clflags.include_dirs),
  "<dir>  Add <dir> to the list of include directories";

  "-nostdlib", Arg.Set Clflags.nostdlib,
  "  do not add default directory to the list of include directories";
(*
  "-oc", Arg.Unit (fun () -> Clflags.output_mode := Clflags.Ocomp_byte),
  " Compile with ocamlc";

  "-of", Arg.Unit (fun () -> Clflags.output_mode := Clflags.Ofile),
  " Output to file (.ml/.mli)";

  "-oo", Arg.Unit (fun () -> Clflags.output_mode := Clflags.Ocomp_asm),
  " Compile with ocamlopt";

  "-op", Arg.Unit (fun () -> Clflags.output_mode := Clflags.Opp),
  " Preprocessor mode";

  "-oq", Arg.Unit (fun () -> Clflags.output_mode := Clflags.Oquiet),
  " No output";
*)
  "-pprint", Arg.String Formatters.parse_pprint,
  "<flags>  Configures pretty print of types.";

  "-runlib", Arg.Unit print_run_library,
  " Print location of run-time library and exit";

  "-stat", Arg.Set Clflags.debug_stats,
  " Print debugging statistics";

  "-stdlib", Arg.Unit print_standard_library,
  " Print location of standard library and exit";

  "-v", Arg.Unit print_version_and_libraries, 
  " Print compiler version and location of libraries and exit";

  "-version", Arg.Unit print_version,
  " Print compiler version and exit";

  "-nopervasives", Arg.Set Clflags.nopervasives, 
  " do not open then Pervasives module";

] 

let usage = "Usage: flocamlc <options> <files>\nOptions are:"

let anonymous = process_file Format.err_formatter



(* *************************************************************************
   Main loop
 *)

let main () =
  try
    Arg.parse options_list anonymous usage;

    (* Statistics *)
    if !Clflags.debug_stats then begin
      Dalton_debug.Stat.output_major stdout;
      Printf.printf "\n";
      flush stdout;
    end


  with x ->
    Errors.report_error Format.err_formatter x;
    exit 2

let _ = Printexc.catch main ()
