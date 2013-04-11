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

(* $Id: flowcamldep.ml,v 1.6 2003/06/26 13:32:54 simonet Exp $ *)
(* Flowcamldep: *)

(* TEMPORARY J'ai changé parse_use_file en parse_implementation.  Je
   ne comprends pas pourquoi OCaml fait comme ça. *)

open Format
open Datastruct
open Location
open Longident
open Parsetree


(* Options *)

let load_path = ref [""]

(* Print the dependencies *)

let find_dependency modname deps =
  let name = String.uncapitalize modname in
  try
    let filename = Misc.find_in_path !load_path (name ^ Config.ext_pint) in
    let basename = Filename.chop_suffix filename Config.ext_pint in
    (basename ^ Config.ext_oint) :: deps
  with Not_found ->
  try
    let filename = 
      Misc.find_in_path !load_path (name ^ Config.ext_pimp)
    in
    let basename = Filename.chop_suffix filename Config.ext_pimp in
    (basename ^ Config.ext_oimp) :: deps
  with Not_found ->
    deps

let (depends_on, escaped_eol) =
  match Sys.os_type with
  | "Unix" | "Win32" | "Cygwin" -> (": ", "\\\n    ")
  | "MacOS" -> ("\196 ", "\182\n    ")
  | _ -> assert false

let print_dependencies target_file deps =
  match deps with
    [] -> ()
  | _ ->
    print_string target_file; print_string depends_on;
    let rec print_items pos = function
      [] -> print_string "\n"
    | dep :: rem ->
        if pos + String.length dep <= 77 then begin
          print_string dep; print_string " ";
          print_items (pos + String.length dep + 1) rem
        end else begin
          print_string escaped_eol; print_string dep; print_string " ";
          print_items (String.length dep + 5) rem
        end in
    print_items (String.length target_file + 2) deps

(* Optionally preprocess a source file *)

let preprocessor = ref None

let preprocess sourcefile =
  match !preprocessor with
    None -> sourcefile
  | Some pp ->
      flush stdout;
      let tmpfile = Filename.temp_file "camlpp" "" in
      let comm = Printf.sprintf "%s %s > %s" pp sourcefile tmpfile in
      if Sys.command comm <> 0 then begin
        Misc.remove_file tmpfile;
        Printf.eprintf "Preprocessing error\n";
        exit 2
      end;
      tmpfile

let remove_preprocessed inputfile =
  match !preprocessor with
    None -> ()
  | Some _ -> Misc.remove_file inputfile

(* Parse a file or get a dumped syntax tree in it *)

exception Outdated_version

let is_ast_file ic ast_magic =
  try
    let buffer = String.create (String.length ast_magic) in
    really_input ic buffer 0 (String.length ast_magic);
    if buffer = ast_magic then true
    else if String.sub buffer 0 9 = String.sub ast_magic 0 9 then
      raise Outdated_version
    else false
  with
    Outdated_version ->
      failwith "Ocaml and preprocessor have incompatible versions"
  | _ -> false

let parse_implementation ic =
  if is_ast_file ic Config.magic_pimp then
    let source_file = input_value ic in
    (* [Ptop_def (input_value ic : Parsetree.structure)] *)
    (input_value ic : Parsetree.implementation)
  else begin
    seek_in ic 0;
    let lb = Lexing.from_channel ic in
    Parse.implementation lb
  end

let parse_interface ic =
  if is_ast_file ic Config.magic_pint then
    let source_file = input_value ic in
    (input_value ic : Parsetree.interface)
  else begin
    seek_in ic 0;
    let lb = Lexing.from_channel ic in
    Parse.interface lb
  end

(* Process one file *)

let error_occurred = ref false

let file_dependencies source_file =
  Location.input_name := source_file;
  if Sys.file_exists source_file then begin
    try
      Depend.free_structure_names := StringSet.empty;
      let input_file = preprocess source_file in
      let ic = open_in_bin input_file in
      try
        if Filename.check_suffix source_file Config.ext_pimp then begin

          let pimp = parse_implementation ic in
	  Depend.add_implementation StringSet.empty pimp;
          let basename = Filename.chop_suffix source_file Config.ext_pimp in

          let init_deps =
            if Sys.file_exists (basename ^ Config.ext_pint) 
	    then [basename ^ Config.ext_oint] else []
	  in

          let deps =
            StringSet.fold find_dependency
	      !Depend.free_structure_names init_deps
	  in

	  print_dependencies (basename ^ Config.ext_oimp) deps

        end 
	else if Filename.check_suffix source_file Config.ext_pint then begin

          let pint = parse_interface ic in
          Depend.add_interface StringSet.empty pint;
          let basename = Filename.chop_suffix source_file Config.ext_pint in
	  
          let deps =	    
            StringSet.fold find_dependency
	      !Depend.free_structure_names []
	  in

	  print_dependencies (basename ^ Config.ext_oint) deps;

        end; 
        close_in ic; remove_preprocessed input_file

      with x ->
        close_in ic; remove_preprocessed input_file;
        raise x

    with x ->
      let report_err = function
      | Lexer.Error(err, start, stop) ->
          fprintf Format.err_formatter "@[%a%a@]@."
          Location.print {loc_start = start; loc_end = stop; loc_ghost = false}
          Lexer.report_error err
      | Syntaxerr.Error err ->
          fprintf Format.err_formatter "@[%a@]@."
          Syntaxerr.report_error err
      | Sys_error msg ->
          fprintf Format.err_formatter "@[I/O error:@ %s@]@." msg
      | x -> raise x in
      error_occurred := true;
      report_err x
  end

(* Entry point *)

let usage = "Usage: flowcamldep <options> <files>\nOptions are:"

let _ =
  Arg.parse [
     "-I", Arg.String(fun dir -> load_path := !load_path @ [dir]),
       "<dir>  Add <dir> to the list of include directories";
(*
     "-pp", Arg.String(fun s -> preprocessor := Some s),
       "<command>  Pipe sources through preprocessor <command>"
*)
    ] file_dependencies usage;
  exit (if !error_occurred then 2 else 0)
