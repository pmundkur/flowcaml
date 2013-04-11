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

(* $Id: flowcamlmerge.ml,v 1.3 2003/06/26 13:32:54 simonet Exp $ *)

(***************************************************************************)
(* Files processing *)

let output_file oc filename =
  let len = 256 in
  let ic = open_in filename in
  let buf = String.create len in
  let rec loop () =
    let i = input ic buf 0 len in
    if i > 0 then begin
      output oc buf 0 i;
      loop ()
    end
  in
  loop ();
  close_in ic



let implementation oc sourcefile =

  let prefixname = Filename.chop_extension sourcefile in
  let modulename = String.capitalize(Filename.basename prefixname) in

  Printf.fprintf oc
  "\n\
  (***************************************************************************)\
  \n\n\
  module %s = struct\n\
  %a\n\
  end\n\n\n" modulename output_file sourcefile;

  ()



let interface oc sourcefile =

  let prefixname = Filename.chop_extension sourcefile in
  let modulename = String.capitalize(Filename.basename prefixname) in

  Printf.fprintf oc
  "\n\
  (***************************************************************************)\
  \n\n\
  module %s : sig\n\
  %a\n\
  end\n\n\n" modulename output_file sourcefile;

  ()



let output_chan = ref stdout

let process_file name =

  if Filename.check_suffix name Config.ext_pimp then
    implementation !output_chan name
  else if Filename.check_suffix name Config.ext_pint then
    interface !output_chan name
  else
    raise(Arg.Bad("don't know what to do with " ^ name))



(***************************************************************************)
(* Command line arguments *)

let print_version () =
  print_string "flowcamlmerge, version ";
  print_endline Config.version;
  exit 0

let options_list = [

  "-o", Arg.String (fun s -> output_chan := open_out s),
  " <file>  Set output file name to <file>";

  "-v", Arg.Unit print_version, 
  "  Print version and exit";

  "-version", Arg.Unit print_version,
  "  Print version and exit";

] 

let usage = "Usage: flocamlc <options> <files>\nOptions are:"

let anonymous = process_file



let main () =
  Arg.parse options_list anonymous usage;
  close_out !output_chan

let _ =
  main ()
