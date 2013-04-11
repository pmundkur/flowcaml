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

(* $Id: doc_gen.ml,v 1.6 2003/06/26 13:32:48 simonet Exp $ *)

open Printf
open Datastruct
open Location
open Parsetree
open Types
open Doc_parsetree
module G = Doc_latex



exception Mismatch of string * Location.t
exception With_constraint of Location.t



(***************************************************************************)
(** {2 Extracting information from the typed tree} *)

module Tbl = StringHashtbl

type components = 
    { values: value_description Tbl.t;
      types: type_declaration Tbl.t;
      levels: level_declaration Tbl.t;
      exceptions: exception_declaration Tbl.t;
      modules: module_type Tbl.t;
      modtypes: modtype_declaration Tbl.t
    }

let extract_components sg =
  let comp =
    { values = Tbl.create 7;
      types = Tbl.create 7;
      levels = Tbl.create 7;
      exceptions = Tbl.create 7;
      modules = Tbl.create 7;
      modtypes = Tbl.create 7
    }
  in

  List.fold_right (fun item () ->
    match item with
      Tsig_value (id, vald) -> Tbl.add comp.values (Ident.name id) vald
    | Tsig_type (id, typd) -> Tbl.add comp.types (Ident.name id) typd
    | Tsig_level (id, lvld) -> Tbl.add comp.levels (Ident.name id) lvld
    | Tsig_exception (id, exn) -> Tbl.add comp.exceptions (Ident.name id) exn
    | Tsig_module (id, modd) -> Tbl.add comp.modules (Ident.name id) modd
    | Tsig_modtype (id, modt) -> Tbl.add comp.modtypes (Ident.name id) modt
  ) sg ();

  comp



(***************************************************************************)

let locations_types loc list =
  let mkloc loc_start loc_end =
    { loc_start = loc_start; loc_end = loc_end; loc_ghost = false }
  in
  let rec fold loc_start = function
      [] -> []
    | [name, decl] -> [name, decl, mkloc loc_start loc.loc_end]
    | (name1, decl1) :: (((_, decl2) :: _) as tl) ->
	(name1, decl1, mkloc loc_start decl2.ptype_loc.loc_start) 
	:: (fold decl1.ptype_loc.loc_end tl)
  in
  fold loc.loc_start list



let rec long_module_type pmty =
  match pmty.pmty_desc with
    Pmty_ident lid -> false
  | Pmty_signature psig -> true
  | Pmty_functor (_, _, _, _, res_pmty, _) -> long_module_type res_pmty
  | Pmty_with _ -> false
  | Pmty_paren pmty' -> long_module_type pmty'

let rec is_functor = function
    Tmty_signature _ -> false
  | Tmty_functor _ -> true
  | Tmty_ident _ -> false (* TEMPORARY assert false *)



let rec signature ppf psig = function

    [] -> ()

  | sg ->

      let comp = extract_components sg in
      let find_comment_forward loc =
	Doc_comments.find_forward (fun ft -> G.comment ppf ft) loc
      in
      let find_comment loc =
	Doc_comments.find (fun ft -> G.comment ppf ft) loc
      in
      let find_tt loc tbl name = 
	try
	  Tbl.find tbl name
	with
	  Not_found -> raise (Mismatch (name, loc))
      in

      G.begin_signature ppf;
      
      begin try

	List.iter (function psig ->
	  begin match psig.psig_desc with
	    Psig_value (name, pval) ->
	      G.value_description ppf name 
		(find_tt psig.psig_loc comp.values name)
		(find_comment psig.psig_loc)
	  | Psig_type list ->
	      List.iter (function (name, decl, loc') ->
		G.type_declaration ppf name
		  (find_tt loc' comp.types name) (find_comment loc')
              ) (locations_types psig.psig_loc list)	  
	  | Psig_level (name, plvd) ->
	      G.level_declaration ppf name
		(find_tt psig.psig_loc comp.levels name)
		(find_comment psig.psig_loc)
	  | Psig_exception (name, pexn) ->
	      G.exception_declaration ppf name
		(find_tt psig.psig_loc comp.exceptions name)
		(find_comment psig.psig_loc)
	  | Psig_module (name, pmty) ->
	      let mty = find_tt psig.psig_loc comp.modules name 
	      and tf = find_comment_forward psig.psig_loc in
	      G.begin_signature_item ppf;
	      modul ppf name pmty mty tf
	  | Psig_modtype (name, pmodtype) ->
	      let tmodtype = find_tt psig.psig_loc comp.modtypes name 
	      and tf = find_comment_forward psig.psig_loc in
	      G.begin_signature_item ppf;
	      modtype ppf psig.psig_loc name pmodtype tmodtype tf
	  | Psig_open lid -> ()
	  | Psig_include pmod -> ()
      
	  end;

        ) psig
      with
	Doc_comments.TerminateComment -> 
	  ()

      end;

      G.end_signature ppf



and modul ppf name pmty mty tf =
  if long_module_type pmty then begin
    G.begin_long ppf 
      (if is_functor mty then "Functor" else "Module")
      name tf;
    G.begin_module ppf name;
    module_type ppf name pmty mty;
    G.end_module ppf [];
    G.end_long ppf
  end
  else begin
    G.begin_module ppf name;
    module_type ppf name pmty mty;
    G.end_module ppf tf
  end

and modtype ppf loc name pmodtype tmodtype tf =
  match pmodtype, tmodtype with
    Pmodtype_abstract, Tmodtype_abstract ->
      G.modtype_abstract ppf name tf
  | Pmodtype_manifest pmty, Tmodtype_manifest mty ->
      if long_module_type pmty then begin
	G.begin_long ppf "Module type" name tf;
	G.begin_modtype ppf name;
	module_type ppf name pmty mty;
	G.end_modtype ppf [];
	G.end_long ppf
      end
      else begin
	G.begin_modtype ppf name;
	module_type ppf name pmty mty;
	G.end_modtype ppf tf
      end
  | _ -> raise (Mismatch (name, loc))

and module_type ppf name pmty mty =
  match pmty.pmty_desc, mty with
    Pmty_ident lid, Tmty_ident path ->
      G.module_ident ppf path
  | Pmty_signature psig, Tmty_signature sg ->
      G.module_sig_begin ppf;
      signature ppf psig sg;
      G.module_sig_end ppf
  | Pmty_functor (arg_name, arg_pmty, _, _, res_pmty, _), 
      Tmty_functor (arg_id, arg_mty, pci, pcf, res_mty) ->
	G.functor_arrow ppf arg_id arg_mty pci pcf;
	module_type ppf name res_pmty res_mty
  | Pmty_with (pmty', list), _ -> 
      begin match pmty'.pmty_desc with
	Pmty_ident lid -> G.with_constraint ppf lid list
      | _ -> raise (With_constraint pmty.pmty_loc)
      end
  | Pmty_paren pmty', _ ->
      module_type ppf name pmty' mty
  | _ -> raise (Mismatch (name, pmty.pmty_loc))

let interface ppf modulename pint int =
  G.begin_interface ppf modulename;
  signature ppf pint.pint_signature int.int_signature;
