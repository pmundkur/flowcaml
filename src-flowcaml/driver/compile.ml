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

(* $Id: compile.ml,v 1.5 2003/06/26 13:32:49 simonet Exp $ *)
(* Compile: the batch compiler *)

open Parsetree
open Types
open Print_types
open Typedtree



(***************************************************************************)
(* Initialisation the list of load paths *)

let init_path () =
  let dirs =
    !Clflags.include_dirs
  in
  let exp_dirs =
    List.map (Misc.expand_directory Config.standard_library) dirs 
  in
  Config.load_path := 
    "" :: List.rev_append exp_dirs (Clflags.std_include_dir ());
  Env.reset_cache()



(***************************************************************************)
(* Preprocessing stuff *)

let preprocess sourcefile =
  match !Clflags.preprocessor with
    None -> sourcefile
  | Some pp ->
      let tmpfile = Filename.temp_file "camlpp" "" in
      let comm = Printf.sprintf "%s %s > %s" pp sourcefile tmpfile in
      if Misc.command comm <> 0 then begin
        Misc.remove_file tmpfile;
        Printf.eprintf "Preprocessing error\n";
        exit 2
      end;
      tmpfile

let remove_preprocessed inputfile =
  match !Clflags.preprocessor with
    None -> ()
  | Some _ -> Misc.remove_file inputfile

let remove_preprocessed_if_ast inputfile =
  match !Clflags.preprocessor with
    None -> ()
  | Some _ -> 
      if inputfile <> !Location.input_name then Misc.remove_file inputfile



(***************************************************************************)
(* Parse a file or get a dumped syntax tree in it *)

exception Outdated_version

let parse_file inputfile parse_fun ast_magic =
  let ic = open_in_bin inputfile in
  let is_ast_file =
    try
      let buffer = String.create (String.length ast_magic) in
      really_input ic buffer 0 (String.length ast_magic);
      if buffer = ast_magic then true
      else if String.sub buffer 0 9 = String.sub ast_magic 0 9 then
        raise Outdated_version
      else false
    with
      Outdated_version ->
        Misc.fatal_error "Ocaml and preprocessor have incompatible versions"
    | _ -> false
  in
  let ast =
    try
      if is_ast_file then begin
	if not !Clflags.dumped_output then
	  Misc.fatal_error
	    "Dumped output required if dumped input";
        Location.input_name := input_value ic;
        input_value ic
      end else begin
        seek_in ic 0;
        Location.input_name := inputfile;
        parse_fun (Lexing.from_channel ic)
      end
    with x -> close_in ic; raise x
  in
  close_in ic;
  ast



(***************************************************************************)
(* Handling interface files *)

let interface ppf sourcefile =

  init_path();
  Stripinfo.reset ();

  let prefixname = Filename.chop_extension sourcefile in
  let modulename = String.capitalize (Filename.basename prefixname) in
  let inputfile = preprocess sourcefile in

  try

    (* Parsing *)
    let pint = 
      parse_file inputfile Parse.interface Config.magic_pint 
    in

    (* Typing *)
    let env = Predef.initial () in
    let int = Comp_unit.transl_interface env pint in

    (* Print the signature on the standard output (-i option) *)
    if !Clflags.print_types then 
      Format.fprintf Format.std_formatter "@[<v>%a@]@." 
	fprint_signature int.int_signature;

    (* Save the interface in .fcmi file *)
    Env.save_interface int modulename (prefixname ^ Config.ext_int);

    (* Output the OCaml source, depending on the mode *)
    let gen_source =
      if !Clflags.dumped_output then Gen_ast.interface
      else Gen_source.interface
    in
    begin match !Clflags.output_mode with
      Clflags.Ofile ->
	let oc = open_out (prefixname ^ Config.ext_oint) in
	gen_source inputfile oc pint;
	close_out oc
    | Clflags.Oquiet -> 
	()
    | Clflags.Opp -> 
	gen_source inputfile stdout pint
    end;

    remove_preprocessed inputfile

  with exn ->
    remove_preprocessed_if_ast inputfile;
    raise exn



(***************************************************************************)
(* Handling implementation files *)

let (++) x f = f x

let print_if ppf flag printer arg =
  if !flag then Format.fprintf ppf "%a@." printer arg;
  arg

let implementation ppf sourcefile =

  init_path();
  Stripinfo.reset ();

  let prefixname = Filename.chop_extension sourcefile in
  let modulename = String.capitalize(Filename.basename prefixname) in
  let inputfile = (* preprocess *) sourcefile in

  try

    (* Parsing *)
    let pimp = 
      parse_file inputfile Parse.implementation Config.magic_pimp
    in

    (* Typing *)
    let env = Predef.initial () in
    let imp = Comp_unit.type_implementation env pimp in

    (* Print the signature on the standard output (-i option) *)
    if !Clflags.print_types then
      Format.fprintf Format.std_formatter "@[<v>@[<v>%a@]@]@."
	fprint_signature imp.imp_interface.int_signature;

    (* Coercion *)
    let coercion =
      if Sys.file_exists (prefixname ^ Config.ext_pint) then begin
	(* If an interface file exists for the compilation unit, the inferred
	   interface is compared to the provided one. *)
	let intf_file =
          try Misc.find_in_path !Config.load_path (prefixname ^ Config.ext_int)
          with Not_found -> prefixname ^ Config.ext_int
	in
	let dclint = Env.read_interface modulename intf_file in
	Comp_unit.included sourcefile imp.imp_interface intf_file dclint
      end 
      else begin
	(* Otherwise, the inferred interface is saved. *)
	Env.save_interface imp.imp_interface modulename 
	  (prefixname ^ Config.ext_int);
	Tcoerce_none
      end
    in

    (* Output the OCaml source, depending on the mode *)
    let gen_source =
      if !Clflags.dumped_output then Gen_ast.implementation
      else Gen_source.implementation
    in
    begin match !Clflags.output_mode with
      Clflags.Ofile ->
	let oc = open_out (prefixname ^ Config.ext_oimp) in
	gen_source inputfile oc pimp;
	close_out oc
    | Clflags.Oquiet ->
	()
    | Clflags.Opp -> 
	gen_source inputfile stdout pimp
    end;

    remove_preprocessed inputfile

  with x ->
    remove_preprocessed_if_ast inputfile;
    raise x
