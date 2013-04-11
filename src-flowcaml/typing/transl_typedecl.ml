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

(* $Id: transl_typedecl.ml,v 1.5 2003/06/26 13:32:58 simonet Exp $ *)
(* Transl_typedecl: checking and translating type declarations *)

open Format
open Dalton_aux
open Fkind
open Longident
open Asttypes
open Parsetree
open Path
open Type_constructor
open Types
open Solver
open Transl_typeexpr



(* *************************************************************************
   Inclusion
 *)

open Include_error

let included env id decl1 decl2 =

  let error report =
    raise(Error[Type_declarations(id, decl1, decl2, report)])
  in

  (* Checking the number of parameters. *)
  let arity1 = List.length decl1.type_kinds
  and arity2 = List.length decl2.type_kinds in
  if arity1 <> arity2 then error (Rtyp_arity_mismatch (arity1, arity2));
    
  (* Checking kinds. *)
  Misc.iter2i (fun i kind1 kind2 ->
    if not (Fkind.equal kind1 kind2) then
      error (Rtyp_kind_mismatch (i, kind1, kind2))
    ) decl1.type_kinds decl2.type_kinds;

  (* Checking props. *)
  Misc.iter2i (fun i (v1, a1) (v2, a2) ->
    if a1 && not a2 then error (Rtyp_annot_mismatch i);
    if not (Variance.leq v1 v2) then 
      error (Rtyp_variance_mismatch (i, v1, v2))
    ) decl1.type_prop decl2.type_prop;

  (* Checking fun. *)
  if decl1.type_fun && not decl2.type_fun then error Rtyp_fun_mismatch;

  (* Checking type representation. *)
  begin match decl1.type_repr, decl2.type_repr with
    (Type_abstract | Type_variant _ | Type_record _), Type_abstract -> ()
  | Type_abstract, (Type_variant _ | Type_record _) 
  | Type_record _, Type_variant _
  | Type_variant _, Type_record _ -> error Rtyp_repr_mismatch
  | Type_variant variants1, Type_variant variants2 ->
      if List.length variants1 <> List.length variants2 then 
	error Rtyp_repr_mismatch;
      Misc.iter2i (fun i (name1, cstr1) (name2, cstr2) ->
	if name1 <> name2 then error Rtyp_repr_mismatch;
	if not (Constructor_description.identical cstr1 cstr2) then
          error Rtyp_repr_mismatch
        ) variants1 variants2
    | Type_record fields1, Type_record fields2 -> 
	if List.length fields1 <> List.length fields2 then 
	  error Rtyp_repr_mismatch;
	Misc.iter2i (fun i (name1, lbl1) (name2, lbl2) ->
	  if name1 <> name2 then error Rtyp_repr_mismatch;
	  if not (Label_description.identical lbl1 lbl2) then
	    error Rtyp_repr_mismatch
        ) fields1 fields2
  end;

  begin match decl2.type_manifest with
    None -> ()
  | Some _ ->
      let decl1' =
	match decl1.type_manifest with
	  Some _ -> decl1
	| None -> 
	    let typ =
	      Solver.typ decl1.type_cset
		{ Solver.constr = { tc_desc = TCpath (Pident id);
				    tc_kinds = decl1.type_kinds;
				    tc_prop = decl1.type_prop;
				    tc_fun = decl1.type_fun
				  } ;
		  Solver.sons = decl1.type_params
		}	
		in
	    { decl1 with type_manifest = Some typ }
      in
      if not (Type_declaration.equivalent decl1' decl2) then
	error Rtyp_repr_mismatch
  end

  

(* *************************************************************************
   Reporting errors
 *)

type error =
    Row_domain of string
  | Contravariant_annot of string
  | Row_annot of string
  | Duplicate_constructor of string
  | Duplicate_label of string
  | Unannotated_multiple_variant
  | Unannotated_mutable_record
  | Recursive_abbrev of string
  | Definition_mismatch



exception Error of Location.t * error

let error (loc, err) = raise (Error (loc, err))



let report_error ppf = function
    Row_domain name ->
      fprintf ppf
	"The domain of the type parameter '%s cannot be inferred in this \
	type declaration" name
  | Contravariant_annot name ->
      fprintf ppf
	"The type parameter '%s is guarded in this declaration and \
	hence cannot appear at contravariant or invariant positions."
	name
  | Row_annot name ->
      fprintf ppf
	"The type parameter '%s has kind row[?] but is guarded \
	in this type declaration" name
  | Duplicate_constructor s ->
      fprintf ppf "Two constructors are named %s" s
  | Duplicate_label s ->
      fprintf ppf "Two labels are named %s" s
  | Unannotated_multiple_variant ->
      fprintf ppf 
	"This variant datatype has several constructors but \
	has no external security level."
  | Unannotated_mutable_record ->
      fprintf ppf 
	"This record datatype has mutable field(s) but \
	has no external security level."
  | Recursive_abbrev s ->
      fprintf ppf "The type abbreviation %s is cyclic" s
  | Definition_mismatch ->
      fprintf ppf
        "The variant or record definition does not match that of the \
	manifest type"



(* *************************************************************************
   Variance/annot/content inference
 *)

let rec prop_type env prop_env constr_env f0 a0 v0 typ =

  let rec prop_type_rec v a typ =

    match typ.ptyp_desc with

      Ptyp_var name ->

	let v', a' = List.assoc name prop_env in
	Variance_solver.leq (Variance_solver.Term v) v';
	Bool_solver.leq (Bool_solver.Term a) a'
	
    | Ptyp_arrow (t1, t2, t3, t4, t5) ->

	let v' = Variance_solver.create (false, false) in
	let a' = Bool_solver.create false in
	Bool_solver.leq (Bool_solver.Term (Bool_solver.create true)) f0;
	Variance_solver.leq (Variance_solver.Opp v) v';
	prop_type_rec v' a' t1;
	prop_type_rec v' a' t2;
	prop_type_rec v a' t3;
	prop_type_rec v a' t4;
	prop_type_rec v a t5

    | Ptyp_tuple list ->

	List.iter (prop_type_rec v a) list
	
    | Ptyp_constr (lid, list) ->

	let constr_fun, constr_prop =
	  try 
	    List.assoc (Longident.lident lid) constr_env
	  with
	    Not_found ->
	      let decl = (snd (Env.lookup_type typ.ptyp_loc lid env)) in
	      Bool_solver.create decl.type_fun,
	      List.map (function v, a ->
		Variance_solver.of_variance v,
		Bool_solver.create a
              ) decl.type_prop
	in

	Bool_solver.leq (Bool_solver.Term constr_fun) f0;

	List.iter2 (fun (v', a') typ' ->
	  let v'' = Variance_solver.create (false, false)
	  and a'' = Bool_solver.create false in
	  Variance_solver.leq (Variance_solver.Combine (v, v')) v'';
	  Bool_solver.leq (Bool_solver.And (a, a')) a'';
	  prop_type_rec v'' a'' typ'
        ) constr_prop list

    | Ptyp_row (lbl, t1, t2) ->

	prop_type_rec v a t1;
	prop_type_rec v a t2

    (* Cannot appear in type definitions *)
    | Ptyp_bounds _ 
    | Ptyp_arrow_abbrev _ -> assert false

    | Ptyp_paren typ' ->
	prop_type env prop_env constr_env f0 a0 v0 typ'

  in

  prop_type_rec
    (Variance_solver.of_variance v0)
    (Bool_solver.create a0)
    typ



(* *************************************************************************
   Topological ordering of type declarations
 *)

let rec predecessors names ptyp =
  match ptyp.ptyp_desc with
    Ptyp_var _ -> []
  | Ptyp_arrow (ptyp1, ptyp2, ptyp3, ptyp4, ptyp5) ->
      (predecessors names ptyp1)
      @ (predecessors names ptyp2)
      @ (predecessors names ptyp3)
      @ (predecessors names ptyp4)
      @ (predecessors names ptyp5)
  | Ptyp_tuple ptyp_list ->
      List.concat (List.map (predecessors names) ptyp_list)
  | Ptyp_constr (lid, ptyp_list) ->
      let tail = List.concat (List.map (predecessors names) ptyp_list) in
      begin match lid with
	Lident s when List.mem s names -> s :: tail
      |	_ -> tail
      end
  | Ptyp_row (_, ptyp1, ptyp2) ->
      (predecessors names ptyp1) @ (predecessors names ptyp2)

  (* Cannot appear in type declarations *)				     
  | Ptyp_bounds _
  | Ptyp_arrow_abbrev _ -> assert false

  | Ptyp_paren ptyp' ->
      predecessors names ptyp'


let tsort ptype_list =

  let names = List.map fst ptype_list in

  List.iter (function ptype_name, ptype ->
    match ptype.ptype_manifest with
      None -> ()
    | Some ptyp -> 
	ptype.ptype_pred <- predecessors names ptyp;
	List.iter (function ptype_name' ->
	  let ptype' = (List.assoc ptype_name' ptype_list) in
	  ptype'.ptype_topo <- ptype'.ptype_topo + 1
	) ptype.ptype_pred
  ) ptype_list;

  let s = Stack.create () in

  List.iter (function ptype_name, ptype ->
    if ptype.ptype_topo = 0 then Stack.push (ptype_name, ptype) s
  ) ptype_list;

  let rec loop () =

    try

      let ptype_name, ptype = Stack.pop s in
      List.iter (function ptype_name' ->
	let ptype' = (List.assoc ptype_name' ptype_list) in
	ptype'.ptype_topo <- ptype'.ptype_topo - 1;
	if ptype.ptype_topo = 0 then Stack.push (ptype_name', ptype') s
      ) ptype.ptype_pred;
      
      (ptype_name, ptype) :: loop ()

    with 
      Stack.Empty -> []

  in

  let ptype_list' = loop () in
  try
    let ptype_name, ptype = 
      List.find (function _, ptype -> ptype.ptype_topo <> 0) ptype_list
    in
    error (ptype.ptype_loc, Recursive_abbrev ptype_name)
    
  with
    Not_found ->
      ptype_list'




(* *************************************************************************
   Checking type re-export
 *)

let check_abbrev env id ptype decl =

  match decl with
    { type_repr = (Type_variant _ | Type_record _) } ->

      begin match ptype.ptype_manifest with

	None -> ()

      |	Some { ptyp_desc = Ptyp_constr (lid, _) } ->
	  let path, decl' = Env.lookup_type ptype.ptype_loc lid env in
	  begin 
	    try included env id 
                (Subst.type_declaration (Subst.add_type id path Subst.identity)
                   decl) decl'
	  with
	    | Include_error.Error _ ->
		error (ptype.ptype_loc, Definition_mismatch)
	  end

      |	Some _ -> error (ptype.ptype_loc, Definition_mismatch)

      end

  | _ -> ()



(* *************************************************************************
   Translating type declarations
 *)

let translate_kind loc env = function
    Pak_var -> Fkind.Ukind.create Fkind.Ukind.Var
  | Pak_level -> Fkind.Ukind.level
  | Pak_type -> Fkind.Ukind.typ
  | Pak_row lid_list -> 
      let path_list = 
	List.map (function lid -> 
	  fst (Env.lookup_exception loc lid env)
        ) lid_list
      in
      Fkind.Ukind.unfreeze (Fkind.Row path_list)



let translate_variance = function
    Pvar_none -> Variance_solver.create (false, false)
  | Pvar_covariant | Pvar_guarded -> Variance_solver.create (true, false)
  | Pvar_contravariant -> Variance_solver.create (false, true)
  | Pvar_invariant -> Variance_solver.create (true, true)


let translate_annot = function
    Pvar_none | Pvar_covariant | Pvar_contravariant | Pvar_invariant -> 
      Bool_solver.create false
  | Pvar_guarded ->
      Bool_solver.create true

let inference env ptype_list =

  (* Creating mutable structures representing kinds & al for each parameter
     of a type declaration. *)

  let params_list = 
    List.map (function type_name, ptype ->
      Bool_solver.create ptype.ptype_fun,
      begin List.map (function param_name, (kind, v) ->
	param_name, (translate_kind ptype.ptype_loc env kind,
		     translate_variance v,
		     translate_annot v)
	) ptype.ptype_params
      end
    ) ptype_list
  in

  (* "Environments" (for kind and prop inference) describing the new type
     names.
   *)

  let kind_constr_env =
    List.map2 (fun (type_name, ptype) (fun_flag, params) ->
      type_name, 
      List.map (function param_name, (k, _, _) -> k) params
    ) ptype_list params_list
  and prop_constr_env =
    List.map2 (fun (type_name, ptype) (fun_flag, params) ->
      type_name,
      (fun_flag, List.map (function param_name, (_, v, a) -> (v, a)) params)
    ) ptype_list params_list
  in

  (* Kinds & al. inference *)

  List.iter2 (fun (type_name, ptype) (fun_flag, params) ->

    let kind_env =
      ref (List.map (function param_name, (k, _, _) -> param_name, k) params)
    and prop_env =
      List.map (function param_name, (_, v, a) -> param_name, (v, a)) params
    in

    begin match ptype.ptype_repr with

      Ptype_abstract -> ()

    | Ptype_variant (variants, opt_level) ->

	Misc.unicity (fun (name1, _) (name2, _) -> 
	  Pervasives.compare name1 name2) variants
	  begin fun (name, _) _ ->
	    error (ptype.ptype_loc, Duplicate_constructor name)
	  end;

	let ext_annot = 
	  match opt_level with
	    None -> 
	      if List.length variants > 1 then 
		error (ptype.ptype_loc, Unannotated_multiple_variant);
	      false
	  | Some (l, _) ->
	      kind_type_expect false env kind_env kind_constr_env Ukind.level l;
	      prop_type env prop_env prop_constr_env fun_flag true Covariant l;
	      true
	in

	List.iter (function constr_name, ptyp_list ->
	  List.iter (function ptyp ->
	    kind_type_expect false env kind_env kind_constr_env Ukind.typ ptyp;
	    prop_type env prop_env prop_constr_env fun_flag (not ext_annot) 
	      Covariant ptyp
          ) ptyp_list
	) variants

    | Ptype_record (fields, opt_level) ->

	Misc.unicity (fun (name1,_,_) (name2,_,_) -> 
	  Pervasives.compare name1 name2) fields
	  begin fun (name, _, _) _ ->
	    error (ptype.ptype_loc, Duplicate_label name)
	  end;

	let ext_annot =
          match opt_level with
	    None -> 
	      if List.exists (function _, mf, _ -> mf = Mutable) fields then
		error (ptype.ptype_loc, Unannotated_mutable_record);
	      false
	  | Some (l, _) ->
	      kind_type_expect false env kind_env kind_constr_env Ukind.level l;
	      prop_type	env prop_env prop_constr_env fun_flag true Covariant l;
	      true
	in

	List.iter (function field_name, mutable_flag, ptyp ->
	  let variance = 
	    match mutable_flag with
	      Mutable -> Invariant
	    | Immutable -> Covariant
	  in
	  kind_type_expect false env kind_env kind_constr_env Ukind.typ ptyp;
	  prop_type env prop_env prop_constr_env fun_flag (not ext_annot) 
	    variance ptyp
        ) fields

    end;

    begin match ptype.ptype_manifest with
      None -> ()
    | Some ptyp ->
	kind_type_expect false env kind_env kind_constr_env Ukind.typ ptyp;
	prop_type env prop_env prop_constr_env fun_flag true Covariant ptyp
    end

  ) ptype_list params_list;

  (* Computing the result. *)

  List.map2 (fun (type_name, ptype) (fun_flag, params) ->

    let params' =

      List.map (function param_name, (k, v, a) ->

	let k' =
	  try Ukind.freeze k with
	    Uset.Freeze -> error (ptype.ptype_loc, Row_domain param_name)
	  | Ukind.Freeze -> Fkind.Type
		
	and v' = Variance_solver.to_variance v
	and a' = Bool_solver.eval a in
	
	if a' && v' <> Covariant then
	  error (ptype.ptype_loc, Contravariant_annot param_name);
	
	begin match k' with
	  Row _ ->
	    if a' then error (ptype.ptype_loc, Row_annot param_name);
	| Level | Type -> ()
	end;

	(param_name, k'), (v', a')

      ) params

    in

    if not (List.exists (function _, (_, a') -> a') params') then
      Bool_solver.leq (Bool_solver.Term (Bool_solver.create true)) fun_flag;

    (Bool_solver.eval fun_flag, params')

  ) ptype_list params_list



let translate env ptype_list =

  let ptype_list = tsort ptype_list in

  let params_list = inference env ptype_list in

  let tdecl_list, fake_env =
    List.fold_right (fun
      ((ptype_name, ptype), (fun_flag, params))
	(decl_list, fake_env) ->
      let id = Ident.create ptype_name in
      let cset = Solver.cset () in
      let node_env = transl_variables cset (List.map fst params) in

      let kinds = List.map (fun ((_, k), _) -> k) params
      and prop = List.map (fun (_, p) -> p) params in

      let decl =
	{ type_cset = cset;
	  type_params = List.map snd node_env;
	  type_kinds = kinds;
	  type_prop = prop;
	  type_fun = fun_flag;
	  type_repr = Type_abstract;
	  type_manifest = 
	  Option.map (transl_type cset fake_env node_env) ptype.ptype_manifest
	}
      in
      ignore (Type_declaration.solve decl);

      ((id, decl) :: decl_list, Env.add_type id decl fake_env)
    ) (List.combine ptype_list params_list) ([], env)
  in

  List.map2 (fun (ptype_name, ptype) (id, decl) ->

    let name_params = List.map fst ptype.ptype_params in

    Stripinfo.store_typedecl
      ptype.ptype_loc 
      begin
	Standard.filter_map2 (fun name kind ->
	  match kind with
	    Level | Row _ -> Some name
	  | Type -> None
        ) name_params decl.type_kinds;
      end;

    let type_repr =

      match ptype.ptype_repr with

	Ptype_abstract -> Type_abstract

      |	Ptype_variant (variants, opt_level) ->

	  Type_variant begin

	    List.map (function variant_name, args ->

	      let decl' = Type_declaration.copy decl in
	      let cset = decl'.type_cset in
	      let node_env = List.combine name_params decl'.type_params in

	      let cstr =
		{ cstr_cset = cset;
		  cstr_args = List.map(transl_type cset fake_env node_env) args;
		  cstr_res = Solver.typ cset
		    { constr = { tc_desc = TCpath (Pident id);
				 tc_kinds = decl'.type_kinds;
				 tc_prop = decl'.type_prop;
				 tc_fun = decl'.type_fun } ;
		      sons = decl'.type_params };
		  cstr_level = 
		  begin match opt_level with
		    None -> None
		  | Some (l, _) -> Some (transl_type cset fake_env node_env l)
		  end;
		  cstr_arity = List.length args
		}
	      in

	      ignore (Constructor_description.solve cstr);

	      variant_name, cstr

            ) variants	    

	  end

      |	Ptype_record (fields, opt_level) ->

	  let all =
	    Array.of_list
	      (List.map (function field_name, _, _ -> field_name) fields)
	  in

	  let pos = ref 0 in

	  let label_descriptions = 

	    List.map (function field_name, mf, arg ->

	      let decl' = Type_declaration.copy decl in
	      let cset = decl'.type_cset in
	      let node_env = List.combine name_params decl'.type_params in

	      let lbl =
		{ lbl_cset = cset;
		  lbl_arg = transl_type cset fake_env node_env arg;
		  lbl_level =
		  begin match opt_level with
		    None -> None
		  | Some (l, _) -> Some (transl_type cset fake_env node_env l)
		  end;
		  lbl_res = Solver.typ cset
		    { constr = { tc_desc = TCpath (Pident id);
				 tc_kinds = decl'.type_kinds;
				 tc_prop = decl'.type_prop;
				 tc_fun = decl'.type_fun } ;
		      sons = decl'.type_params };
		  lbl_all = all;
		  lbl_pos = !pos;
		  lbl_mut = mf
		} 
	      in

	      ignore (Label_description.solve lbl);
	      incr pos;

	      field_name, lbl

            ) fields

	  in

	  Type_record label_descriptions

    in

    let new_decl = { decl with type_repr = type_repr } in

    check_abbrev env id ptype new_decl;

    id, new_decl

  ) (List.rev ptype_list) (List.rev tdecl_list)
  


let enter env ptype_list =

  let decl_list = translate env ptype_list in
  let newenv = 
    List.fold_left (fun newenv (id, desc) ->
      Env.add_type id desc newenv
    ) env decl_list
  in
  decl_list, newenv



(* TEMPORARY Est-ce correct ? *)
let translate_with_constraint env pdecl =

  snd (List.hd (translate env ["%translate_with_constraint", pdecl]))


