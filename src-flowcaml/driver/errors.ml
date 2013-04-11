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

(* $Id: errors.ml,v 1.4 2003/06/30 18:05:33 simonet Exp $ *)
(* Errors: reporting errors *)


open Format

(* Report an error *)

let report_error ppf exn =

  let report ppf = function

    | Misc.Error err ->
	Misc.report_error ppf err

    | Lexer.Error(err, start, stop) ->
	Location.print ppf {Location.loc_start = start;
                            Location.loc_end = stop;
                            Location.loc_ghost = false};
	Lexer.report_error ppf err

    | Syntaxerr.Error err ->
	Syntaxerr.report_error ppf err

    | Env.Error err ->
	Env.report_error ppf err

    | Env.LookupError (loc, err) ->
	Location.print ppf loc; Env.report_lookup_error ppf err

    | Transl_leveldecl.Error (loc, err) ->
	Location.print ppf loc; Transl_leveldecl.report_error ppf err

    | Transl_typeexpr.Error (loc, err) ->
	Location.print ppf loc; Transl_typeexpr.report_error ppf err

    | Transl_typedecl.Error (loc, err) ->
	Location.print ppf loc; Transl_typedecl.report_error ppf err

    | Transl_exception.Error (loc, err) ->
	Location.print ppf loc; Transl_exception.report_error ppf err

    | Type_core.Error (loc_list, err) ->
	List.iter (Location.print ppf) loc_list; 
	Type_core.report_error ppf err

    | Type_mod.Error (loc, err) ->
	Location.print ppf loc; Type_mod.report_error ppf err

    | Include_error.Error (err_list) ->
	Include_error.report_error ppf err_list

    | Comp_unit.Comparison_error (impl_name, intf_name, desc) ->
	Comp_unit.report_error ppf impl_name intf_name desc

    | Sys_error msg ->
	fprintf ppf "I/O error: %s" msg

    | Avl_graphics.Graphic_failure msg ->
	fprintf ppf "Graphic failure: %s" msg

    | x -> fprintf ppf "@]"; raise x

  in

  fprintf ppf "@[%a@]@." report exn


