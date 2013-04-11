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

(* $Id: toploop.ml,v 1.13 2003/06/30 18:05:33 simonet Exp $ *)
(* Toploop: interactive toplevel loop *)

open Parsetree
open Types
open Typedtree
open Print_types



(***************************************************************************)
(* Toplevel directives *)

type directive_fun =
   | Directive_none of (Format.formatter -> Env.t -> unit -> unit)
   | Directive_string of (Format.formatter -> Env.t -> string -> unit)
   | Directive_int of (Format.formatter -> Env.t -> int -> unit)
   | Directive_ident of (Format.formatter -> Env.t -> Longident.t -> unit)
   | Directive_bool of (Format.formatter -> Env.t -> bool -> unit)

let directive_table = (Hashtbl.create 13 : (string, directive_fun) Hashtbl.t)

let register_directive name directive_fun =
  Hashtbl.add directive_table name directive_fun

let process_directive ppf env name arg =
  try
    begin match Hashtbl.find directive_table name, arg with
      Directive_none f, Pdir_none -> f ppf env ()
    | Directive_string f, Pdir_string s -> f ppf env s
    | Directive_int f, Pdir_int i -> f ppf env i
    | Directive_ident f, Pdir_ident lid -> f ppf env lid
    | Directive_bool f, Pdir_bool b -> f ppf env b
    | _ ->
	Format.fprintf ppf "Wrong type of argument for directive `%s'.@." name
    end
  with
    Not_found -> 
      Format.fprintf ppf "Unknown directive `%s'.@." name;
      ()

exception Reset_context

let reset_context ppf env () =
  raise Reset_context

let directive_pprint ppf env flags =
  Formatters.parse_pprint flags

let directive_graph ppf env () =
  Clflags.graph := true;
  Topgraph.state ()

let directive_nograph ppf env () =
  Clflags.graph := false;
  Topgraph.state ()

let directive_lookup parse lookup fprint ppf env string =
  let lexbuf = Lexing.from_string string in
  try
    let lid = parse Lexer.token lexbuf in
    let _, decl = lookup Location.none lid env in
    Format.fprintf ppf "@[%a@]@."
      (fprint (Ident.create (Longident.to_string lid))) decl;
  with
    Parsing.Parse_error ->
      Format.fprintf ppf "@[Syntax error in the identifier.@]@."
      
let directive_lookup_value = 
  directive_lookup Parser.val_longident (fun loc lid env -> 
    match Env.lookup_value loc lid env with
      _, None -> raise Not_found
    | p, Some vald -> 
	Topgraph.signature 
	  [Types.Tsig_value (Ident.create (Longident.to_string lid), vald)];
	p, vald
  ) fprint_value_description
      
let directive_lookup_type = 
  directive_lookup Parser.type_longident Env.lookup_type fprint_type_declaration
      
let directive_lookup_level = 
  directive_lookup Parser.level_longident Env.lookup_level fprint_level_declaration
      
let directive_lookup_exception = 
  directive_lookup Parser.exception_longident
    Env.lookup_exception fprint_exception_declaration

let _ = 
  register_directive "quit" (Directive_none (fun _ _ _ -> exit 0));
  register_directive "reset_context" (Directive_none reset_context);
  register_directive "pprint" (Directive_string directive_pprint);
  register_directive "open_graph" (Directive_none directive_graph);
  register_directive "close_graph" (Directive_none directive_nograph);
  register_directive "lookup_value" (Directive_string directive_lookup_value);
  register_directive "lookup_type" (Directive_string directive_lookup_type);
  register_directive "lookup_level" (Directive_string directive_lookup_level);
  register_directive "lookup_exception"
    (Directive_string directive_lookup_exception);
  ()



(***************************************************************************)

(* Reading function for interactive use *)

let first_line = ref true
let got_eof = ref false;;

let refill_lexbuf buffer len =
  if !got_eof then (got_eof := false; 0) else begin
    let prompt =
      if !first_line then "\n# "
      else if Lexer.in_comment () then "* "
      else "  "
    in
    output_string stdout prompt; flush stdout;
    first_line := false;
    let i = ref 0 in
    try
      while true do
        if !i >= len then raise Exit;
        let c = input_char stdin in
        buffer.[!i] <- c;
        incr i;
        if c = '\n' then raise Exit;
      done;
      !i
    with
    | End_of_file ->
        Location.echo_eof ();
        if !i > 0 then (got_eof := true; !i) else 0
    | Exit -> !i
  end

(* Discard everything already in a lexer buffer *)

let empty_lexbuf lb =
  lb.Lexing.lex_curr_pos <- 0;
  lb.Lexing.lex_abs_pos <- 0;
  lb.Lexing.lex_buffer_len <- 0

(* The interactive loop *)

exception PPerror

let loop ppf =
  Format.fprintf ppf "        Flow Caml version %s@.@." Config.version;
  let lb = Lexing.from_function refill_lexbuf in
  Location.input_name := "";
  Location.input_lexbuf := Some lb;
  Sys.catch_break true;
  Config.load_path := [ Config.standard_library ];
  Config.load_path := "" :: (List.rev !Clflags.include_dirs @ !Config.load_path);

  Topgraph.state ();

  let env = ref (Predef.initial ()) in
  let pc = ref Solver.Lb.bottom in

  while true do
    try
      empty_lexbuf lb;
      Location.reset();
      first_line := true;

      print_string (!Formatters.terminfo).Formatters.input_open;
      let phr = try Parse.toplevel_phrase lb with Exit -> raise PPerror in
      print_string (!Formatters.terminfo).Formatters.input_close;

      match phr with

	Ptop_def structure ->

	  let pci, pcf, new_env =
	    match structure with
	      [ { pstr_desc = Pstr_eval pexp; pstr_loc = loc } ] ->
		let exp, pci, pcf = Type_core.type_eval !env loc pexp in
		Type_mod.check_pc loc !pc pci;

		let vald = 
		  { val_cset = exp.exp_cset;
		    val_context = Expr_context.empty;
		    val_typ = exp.exp_typ
		  }
		in

		Format.printf "@[- : %a@]@." Value_description.fprint vald;
		Topgraph.eval vald;
		pci, pcf, !env

	    | _ ->
		let str, sg, pci, pcf, new_env = 
		  Type_mod.type_structure !env !pc structure 
		in
		Format.printf "@[%a@]@." fprint_signature sg;
		Topgraph.signature sg;
		pci, pcf, new_env
	  in

	  env := new_env;

	  if not (Solver.Lb.leq pcf !pc) then begin
	    pc := Solver.Lb.union pcf !pc;
	    Format.printf "@[<hv>Current evaluation context has level@ %a@]@."
	      Level.Set.fprint !pc
	  end

      |	Ptop_flow plat -> 
	  Principal.translate_into !Env.current_lat plat;
	  Topgraph.flow !Env.current_lat

      |	Ptop_dir (name, arg) ->
	  process_directive Format.std_formatter !env name arg

    with
      Reset_context ->
	pc := Solver.Lb.bottom;
	Format.printf "@[<hv>Level of evaluation context reset@]@."

    | End_of_file -> exit 0
    | Sys.Break -> Format.fprintf ppf "Interrupted.@."
    | PPerror -> ()
    | x -> Errors.report_error ppf x
  done
