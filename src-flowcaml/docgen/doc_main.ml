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

(* $Id: doc_main.ml,v 1.6 2003/06/26 13:32:48 simonet Exp $ *)

open Format


(***************************************************************************)

type output_mode =
    Channel of bool * out_channel
  | Directory of string

let output_mode = ref (Channel (false, stdout))

let close_output () =
  match !output_mode with
    Channel (true, oc) -> close_out oc
  | Channel (false, _) | Directory _ -> ()

let set_output_directory dirname =
  close_output ();
  output_mode := Directory dirname;
  Doc_images.dirname := dirname
      

let set_output_file filename =
  let oc = open_out filename in
  output_mode := Channel (true, oc);
  Doc_images.dirname := Filename.dirname filename




(***************************************************************************)

let interface filename =

  let prefixname = Filename.chop_extension filename in
  let modulename = String.capitalize (Filename.basename prefixname) in
  let intf_file = prefixname ^ Config.ext_int in

  Doc_comments.extract filename;
  Doc_images.set_module modulename;

  let pint = 
    Compile.parse_file filename Parse.interface Config.magic_pint
  in
  let int = Env.read_interface modulename intf_file in

  let oc, close =
    match !output_mode with
      Channel (_, oc) -> oc, false
    | Directory dirname ->
	open_out
	  (Filename.concat dirname (Filename.basename prefixname ^ ".tex")),
	true
  in
  let ppf = formatter_of_out_channel oc in
  Doc_latex.equip_formatter ppf;
  fprintf ppf "@[<v>";
  Doc_gen.interface ppf modulename pint int;
  fprintf ppf "@]@.";

  if close then close_out oc else fprintf ppf "@\n@\n@\n"



let main () =
  Arg.parse
    ["-d",
     Arg.String set_output_directory,
     "<directory>  Output the documentation of compilation unit <name> in \
       <directory>/<name>.tex";

     "-eps",
     Arg.Set Doc_images.eps_output,
     "  Outputs graphical representation of schemes in EPS format";

     "-png",
     Arg.Set Doc_images.png_output,
     "  Outputs graphical representation of schemes in PNG format";

     "-id",
     Arg.String (fun s -> Doc_images.subdirname := s),
     "<directory>  Sets the directory where images are output";
       
     "-o",
     Arg.String set_output_file,
     "<file>  Output documentation in <file>."
   ]
    interface
    "";

  close_output ()



let () = 
  try 
    main ()
  with x when false ->
    Errors.report_error Format.err_formatter x;
    exit 2

