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

(* $Id: gen_ast.ml,v 1.11 2003/06/27 13:09:57 simonet Exp $ *)
(* Gen_ast: *)

open Datastruct
open Longident
open Location
open Asttypes
open Stripinfo
open Parsetree



(***************************************************************************)
(* Core language *)

(* Types *)

let constant = function
    Const_int _
  | Const_char _
  | Const_string _
  | Const_float _ as c -> c
  | Const_charray (s, _) -> Const_string s



let rec core_type info ptyp =

  match ptyp.ptyp_desc with

    Ptyp_arrow_abbrev (args, _, _, res) ->
      List.fold_right (fun arg ptyp' ->
	{ Oast.ptyp_desc = Oast.Ptyp_arrow ("", core_type info arg, ptyp');
	  Oast.ptyp_loc = ptyp.ptyp_loc
	} 
      ) args (core_type info res)

  | Ptyp_paren ptyp' -> core_type info ptyp'

  | _ ->
      { Oast.ptyp_desc = core_type_desc info ptyp.ptyp_desc;
	Oast.ptyp_loc = ptyp.ptyp_loc
      }	

and core_type_desc info = function
    Ptyp_var name when name.[0] = '%' -> 
      Oast.Ptyp_any
  | Ptyp_var name ->
      begin try Oast.Ptyp_var (StringMap.find name info.skel)
      with Not_found -> Oast.Ptyp_var name
      end
  | Ptyp_arrow (ptyp1, _, _, ptyp2, _) ->
      Oast.Ptyp_arrow ("", core_type info ptyp1, core_type info ptyp2)
  | Ptyp_tuple ptyp_list -> 
      Oast.Ptyp_tuple (List.map (core_type info) ptyp_list)
  | Ptyp_constr (lid, ptyp_list) ->
      let ptyp_list' = 
	Standard.filter_map (function ptyp ->
	  if is_atomic info ptyp then None
	  else Some (core_type info ptyp)
        ) ptyp_list
      in
      Oast.Ptyp_constr (lid, ptyp_list')
  | Ptyp_bounds _
  | Ptyp_row _ 
  | Ptyp_arrow_abbrev _
  | Ptyp_paren _ -> 
      assert false



let core_scheme ptys =
  let info = find_scheme ptys.ptys_loc.loc_start in
  core_type info ptys.ptys_type



(* Patterns *)

let rec pattern ppat =

  match ppat.ppat_desc with

    Ppat_paren ppat' -> pattern ppat'

  | _ ->
      { Oast.ppat_desc = pattern_desc ppat.ppat_desc;
	Oast.ppat_loc = ppat.ppat_loc
      }	

and pattern_desc = function
    Ppat_any -> Oast.Ppat_any
  | Ppat_var name -> Oast.Ppat_var name
  | Ppat_alias (ppat, name) -> Oast.Ppat_alias (pattern ppat, name)
  | Ppat_constant c -> Oast.Ppat_constant (constant c)
  | Ppat_tuple ppat_list -> Oast.Ppat_tuple (List.map pattern ppat_list)
  | Ppat_construct (lid, opt_ppat, b) ->
      Oast.Ppat_construct (lid, Option.map pattern opt_ppat, b)
  | Ppat_record list ->
      Oast.Ppat_record
	((List.map (function lid, ppat -> lid, pattern ppat)) list)
  | Ppat_array ppat_list -> Oast.Ppat_array (List.map pattern ppat_list)
  | Ppat_or (ppat1, ppat2) -> Oast.Ppat_or (pattern ppat1, pattern ppat2)
  | Ppat_constraint (ppat, ptys) ->
      Oast.Ppat_constraint (pattern ppat, core_scheme ptys)
  | Ppat_paren _ -> assert false



(* Try patterns *)

let try_counter = ref 0

let try_pattern_item ptryi =
  { Oast.ppat_desc = 
    Oast.Ppat_construct (ptryi.ptryi_exception, 
			 Option.map pattern ptryi.ptryi_arg,
			 false);
    Oast.ppat_loc = ptryi.ptryi_loc
  } 


let try_pattern ident ptry =

  match ptry.ptry_desc with 
    Ptry_any ->
      { Oast.ppat_desc = Oast.Ppat_var ident;
	Oast.ppat_loc = ptry.ptry_loc
      }	
  | Ptry_list [] -> assert false
  | Ptry_list (hd :: tl) ->
      let ppat_or =
	List.fold_left (fun ppat ptryi ->
  	  { Oast.ppat_desc = Oast.Ppat_or (ppat, try_pattern_item ptryi);
	    Oast.ppat_loc = ptry.ptry_loc
	  } 
        ) (try_pattern_item hd) tl
      in
      { Oast.ppat_desc = Oast.Ppat_alias (ppat_or, ident);
	Oast.ppat_loc = ptry.ptry_loc
      }	



(* Expressions *)

let mkexp d =
  { Oast.pexp_desc = d; Oast.pexp_loc = Location.none }

let raise_ident =
  mkexp (Oast.Pexp_ident (Lident "raise"))

let propagate_ident = 
  mkexp (Oast.Pexp_ident (Lident Oast.propagate))

let catchable_ident =
  mkexp (Oast.Pexp_ident (Lident Oast.catchable))

let try_finally_ident =
  mkexp (Oast.Pexp_ident (Lident Oast.try_finally))

let abstract pexp =
  { Oast.pexp_desc = Oast.Pexp_function ("", None,
					 [{ Oast.ppat_desc = Oast.Ppat_any;
					   Oast.ppat_loc = Location.none },
					 pexp]);
    Oast.pexp_loc = pexp.Oast.pexp_loc
  } 



let rec expr pexp =

  match pexp.pexp_desc with

    Pexp_paren pexp' -> expr pexp'


  | _ ->

      { Oast.pexp_desc = expr_desc pexp.pexp_desc;
	Oast.pexp_loc = pexp.pexp_loc
      }	

and expr_desc = function
    Pexp_ident lid -> Oast.Pexp_ident lid
  | Pexp_constant c -> Oast.Pexp_constant (constant c)
  | Pexp_let (flag, list, pexp) ->
      Oast.Pexp_let (flag, patexp_list list, expr pexp)
  | Pexp_function list -> 
      Oast.Pexp_function ("", None, patwhenexp_list list)
  | Pexp_apply (pexp, pexp_list) ->
      Oast.Pexp_apply (expr pexp, List.map (fun pexp -> "", expr pexp) pexp_list)
  | Pexp_match (pexp, list) ->
      Oast.Pexp_match (expr pexp, patwhenexp_list list)
  | Pexp_raise (lid, opt_pexp) -> 
      Oast.Pexp_apply (raise_ident, 
		       ["",
			mkexp (Oast.Pexp_construct (lid,
					    Option.map expr opt_pexp, false))])

  | Pexp_try (pexp, list) -> 
      let pexp' = expr pexp in
      incr try_counter;
      let name = "_exn" ^ (string_of_int !try_counter) in
      let list' =
	List.map (function try_pat, pexp0, try_case ->
	  let ppat' = try_pattern name try_pat in
	  let pexp1 = expr pexp0 in
	  let pexp2 =
	    match try_case with
	      Throw -> pexp1
	    | Propagate _ ->
		mkexp
		(Oast.Pexp_apply (propagate_ident,
				 ["", abstract pexp1; "",
				  mkexp (Oast.Pexp_ident (Lident name))]))
	  in
	  let pexp3 =
	    mkexp (Oast.Pexp_when (
              mkexp (Oast.Pexp_apply (catchable_ident,
	          ["", mkexp (Oast.Pexp_ident (Lident name))])), pexp2))
	  in
	  (ppat', pexp3)
        ) list
      in
      decr try_counter;
      Oast.Pexp_try (pexp', list')

  | Pexp_finally (pexp1, pexp2) ->
      Oast.Pexp_apply (try_finally_ident,
		       ["", abstract (expr pexp1); "", abstract (expr pexp2)])

  | Pexp_tuple pexp_list ->
      Oast.Pexp_tuple (List.map expr pexp_list)
  | Pexp_construct (lid, opt_pexp, b) ->
      Oast.Pexp_construct (lid, Option.map expr opt_pexp, b)
  | Pexp_record (list, opt_pexp) ->
      Oast.Pexp_record (List.map (function lid, pexp -> lid, expr pexp) list,
			Option.map expr opt_pexp)
  | Pexp_field (pexp, lid) -> 
      Oast.Pexp_field (expr pexp, lid)
  | Pexp_setfield (pexp1, lid, pexp2) -> 
      Oast.Pexp_setfield (expr pexp1, lid, expr pexp2)
  | Pexp_array list ->
      Oast.Pexp_array (List.map expr list)
  | Pexp_ifthenelse (pexp1, pexp2, opt_pexp) ->
      Oast.Pexp_ifthenelse (expr pexp1, expr pexp2, Option.map expr opt_pexp)
  | Pexp_sequence (pexp1, pexp2) ->
      Oast.Pexp_sequence (expr pexp1, expr pexp2)
  | Pexp_while (pexp1, pexp2) ->
      Oast.Pexp_while (expr pexp1, expr pexp2)
  | Pexp_for (name, pexp1, pexp2, dir, pexp3) ->
      Oast.Pexp_for (name, expr pexp1, expr pexp2, dir, expr pexp3)
  | Pexp_constraint (pexp, ptys) -> 
      Oast.Pexp_constraint (expr pexp, Some (core_scheme ptys), None)
  | Pexp_assert pexp -> Oast.Pexp_assert (expr pexp)
  | Pexp_assertfalse -> Oast.Pexp_assertfalse
  | Pexp_paren _ -> assert false


and patexp_list list =
  List.map (function ppat, pexp -> pattern ppat, expr pexp) list

and patwhenexp_list list =
  List.map (function ppat, opt_pexp, pexp -> 
    pattern ppat, 
    match opt_pexp with 
      None -> expr pexp
    | Some pexp' -> 
      { Oast.pexp_desc = Oast.Pexp_when (expr pexp', expr pexp);
	Oast.pexp_loc = pexp.pexp_loc
      }	

  ) list



(***************************************************************************)

let value_description pval =
  { Oast.pval_type = core_scheme pval.pval_type;
    Oast.pval_prim = pval.pval_prim
  } 

(* TEMPORARY Vérifier avec Xavier *)
let variance = function
    Pvar_none -> false, false
  | Pvar_covariant | Pvar_guarded -> true, false
  | Pvar_contravariant -> false, true
  | Pvar_invariant -> true, true

let type_repr info = function

    Ptype_abstract -> Oast.Ptype_abstract

  | Ptype_variant (list, _) ->
      Oast.Ptype_variant begin
	List.map (function name, ptyp_list ->
	  name, List.map (core_type info) ptyp_list
        ) list
      end

  | Ptype_record (list, _) ->
      Oast.Ptype_record begin
	List.map (function name, mf, ptyp -> name, mf, core_type info ptyp) list
      end


let type_declaration ptype =

  let info = find_typedecl ptype.ptype_loc.loc_start in
  
  { Oast.ptype_params = List.map fst ptype.ptype_params;
    Oast.ptype_cstrs = []; (* TEMPORARY *)
    Oast.ptype_kind = type_repr info ptype.ptype_repr;
    Oast.ptype_manifest = Option.map (core_type info) ptype.ptype_manifest;
    Oast.ptype_variance = 
      List.map (function _, (_, v) -> variance v) ptype.ptype_params;
    Oast.ptype_loc = ptype.ptype_loc
  } 



(* Type expressions for the module language *)

let rec module_type pmty =
  match pmty.pmty_desc with
    Pmty_paren pmty' -> module_type pmty'
  | _ ->
      { Oast.pmty_desc = module_type_desc pmty.pmty_desc;
	Oast.pmty_loc = pmty.pmty_loc
      } 

and module_type_desc = function
    Pmty_ident lid -> Oast.Pmty_ident lid
  | Pmty_signature psig -> Oast.Pmty_signature (signature psig)
  | Pmty_functor (name, pmty1, _, _, pmty2, _) ->
      Oast.Pmty_functor (name, module_type pmty1, module_type pmty2)
  | Pmty_with (pmty, list) ->
      Oast.Pmty_with (module_type pmty, with_constraints list)
  | Pmty_paren pmty -> assert false



and signature = function
    [] -> []
  | { psig_desc = Psig_level _ } :: psig -> signature psig
  | { psig_desc = desc; psig_loc = loc } :: psig ->
      { Oast.psig_desc = signature_item_desc desc;
	Oast.psig_loc = loc
      } :: signature psig



and signature_item_desc = function
  | Psig_value (name, pval) ->
      Oast.Psig_value (name, value_description pval)
  | Psig_type list ->
      Oast.Psig_type 
	(List.map (function name, ptype -> name, type_declaration ptype) list)
  | Psig_level _ -> assert false
  | Psig_exception (name, pexn) ->
      let opt_param, list = pexn.pexn_type in
      let info =
	{ atomic = 
	    begin match opt_param with
	      None -> StringSet.empty
	    | Some (name, _) -> StringSet.singleton name
	    end;
	  skel = StringMap.empty
	} 
      in
      Oast.Psig_exception (name, List.map (core_type info) list)
  | Psig_module (name, pmty) ->
      Oast.Psig_module (name, module_type pmty)
  | Psig_modtype (name, decl) ->
      Oast.Psig_modtype (name, modtype_declaration decl)
  | Psig_open lid ->
      Oast.Psig_open lid
  | Psig_include pmty ->
      Oast.Psig_include (module_type pmty)



and modtype_declaration = function
    Pmodtype_abstract -> Oast.Pmodtype_abstract
  | Pmodtype_manifest pmty -> Oast.Pmodtype_manifest (module_type pmty)



and with_constraints list = 
  List.fold_right (fun pwth accu ->
    match pwth.pwth_desc with
      Pwith_type decl ->
	(pwth.pwth_ident, Oast.Pwith_type (type_declaration decl)) :: accu
    | Pwith_level _ ->
	accu
    | Pwith_module lid ->
	(pwth.pwth_ident, Oast.Pwith_module lid) :: accu
  ) list []



(* Module expressions *)

and module_expr pmod =

  match pmod.pmod_desc with

    Pmod_paren pmod' -> module_expr pmod'

  | _ ->
      { Oast.pmod_desc = module_expr_desc pmod.pmod_desc;
	Oast.pmod_loc = pmod.pmod_loc
      }	



and module_expr_desc = function
    Pmod_ident lid -> Oast.Pmod_ident lid
  | Pmod_structure pstr -> Oast.Pmod_structure (structure pstr)
  | Pmod_functor (name, pmty, pmod) ->
      Oast.Pmod_functor (name, module_type pmty, module_expr pmod)
  | Pmod_apply (pmod1, pmod2) ->
      Oast.Pmod_apply (module_expr pmod1, module_expr pmod2)
  | Pmod_constraint (pmod, pmty) ->
      Oast.Pmod_constraint (module_expr pmod, module_type pmty)
  | Pmod_paren _ -> assert false



and structure = function
    [] -> []
  | { pstr_desc = Pstr_level _ } :: pstr -> structure pstr
  | { pstr_desc = desc; pstr_loc = loc } :: pstr ->
      { Oast.pstr_desc = structure_item_desc desc;
	Oast.pstr_loc = loc
      } :: structure pstr



and structure_item_desc = function
    Pstr_eval pexp -> Oast.Pstr_eval (expr pexp)
  | Pstr_value (flag, list) -> Oast.Pstr_value (flag, patexp_list list)
  | Pstr_primitive (name, pval) ->
      Oast.Pstr_primitive (name, value_description pval)
  | Pstr_type list ->
      Oast.Pstr_type 
	(List.map (function name, ptype -> name, type_declaration ptype) list)
  | Pstr_level _ -> assert false
  | Pstr_exception (name, pexn) ->
      begin match pexn.pexn_manifest with
	None ->
	  let opt_param, list = pexn.pexn_type in
	  let info =
	    { atomic = 
	      begin match opt_param with
		None -> StringSet.empty
	      | Some (name, _) -> StringSet.singleton name
	      end;
	      skel = StringMap.empty
	    } 
	  in
	  Oast.Pstr_exception (name, List.map (core_type info) list)
      |	Some (lid, _) ->
	  Oast.Pstr_exn_rebind (name, lid)
      end
  | Pstr_module (name, pmod) -> Oast.Pstr_module (name, module_expr pmod)
  | Pstr_modtype (name, pmty) -> Oast.Pstr_modtype (name, module_type pmty)
  | Pstr_open lid -> Oast.Pstr_open lid
  | Pstr_include pmod -> Oast.Pstr_include (module_expr pmod)



(***************************************************************************)
(* Entry points *)

let interface source_file output_chan ast =
  sort ();
  output_string output_chan Config.magic_oint;
  output_string output_chan source_file;
  output_value output_chan begin
    { Oast.psig_desc = Oast.Psig_open (Lident Oast.flowperv);
      Oast.psig_loc = Location.none
    } :: signature ast.pint_signature
  end;
  flush output_chan

let implementation source_file output_chan ast =
  sort ();
  output_string output_chan Config.magic_oimp;
  output_string output_chan source_file;
  output_value output_chan begin
    { Oast.pstr_desc = Oast.Pstr_open (Lident Oast.flowperv);
      Oast.pstr_loc = Location.none
    } :: structure ast.pimp_structure
  end;
  flush output_chan
