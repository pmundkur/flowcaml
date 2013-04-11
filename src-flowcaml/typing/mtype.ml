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

(* $Id: mtype.ml,v 1.6 2003/06/26 13:32:58 simonet Exp $ *)
(* Mtype: Operations on module types *)

open Path
open Type_constructor
open Level
open Types
open Solver



(*************************************************************************)
(** {2 Strengthening} *)

let rec scrape env mty =
  match mty with
    Tmty_ident p ->
      begin try
        scrape env (Env.find_modtype_expansion p env)
      with Not_found ->
        mty
      end
  | _ -> mty



let rec strengthen env mty p =
  match scrape env mty with
    Tmty_signature sg ->
      Tmty_signature(strengthen_sig env sg p)
  | Tmty_functor(param, arg, pci, pcf, res) ->
      Tmty_functor(param, arg, pci, pcf, 
		   strengthen env res (Papply(p, Pident param)))
  | mty ->
      mty

and strengthen_sig env sg p =
  match sg with
    [] -> []
  | (Tsig_value(id, desc) as sigelt) :: rem ->
      sigelt :: strengthen_sig env rem p
  | Tsig_type(id, decl) :: rem ->
      let newdecl =
        match decl.type_manifest with
          None ->
            { decl with
	      type_manifest = Some(
	      Solver.typ decl.type_cset 
		{ constr = { tc_desc = TCpath (Pdot(p, Ident.name id, nopos));
			     tc_kinds = decl.type_kinds;
			     tc_prop = decl.type_prop;
			     tc_fun = decl.type_fun } ;
		  sons = decl.type_params })
	    } 
        | Some _ -> decl 
      in
      Tsig_type(id, newdecl) :: strengthen_sig env rem p
  | Tsig_level(id, decl) :: rem ->
      let level = Tlvl_path (Pdot(p, Ident.name id, nopos)) in
      let newdecl =
	{ lvd_lb = Level.Set.add level decl.lvd_lb;
	  lvd_ub = Level.Set.add level decl.lvd_ub;
	  lvd_lb_closed = Level.Set.add level decl.lvd_lb_closed;
	  lvd_ub_closed = Level.Set.add level decl.lvd_ub_closed;
	} 
      in
      Tsig_level(id, newdecl) :: strengthen_sig env rem p
  | Tsig_exception(id, decl) :: rem ->
      let newdecl =
	match decl.exn_repr with
	  Exn_manifest _ -> decl
	| Exn_abstract ->
	    { decl with
	      exn_repr = Exn_manifest (Pdot(p, Ident.name id, nopos))
	    } 
      in
      Tsig_exception(id, newdecl) :: strengthen_sig env rem p
  | Tsig_module(id, mty) :: rem ->
      Tsig_module(id, strengthen env mty (Pdot(p, Ident.name id, nopos))) ::
      strengthen_sig (Env.add_module id mty env) rem p
      (* Need to add the module in case it defines manifest module types *)
  | Tsig_modtype(id, decl) :: rem ->
      let newdecl =
        match decl with
          Tmodtype_abstract ->
            Tmodtype_manifest(Tmty_ident(Pdot(p, Ident.name id, nopos)))
        | Tmodtype_manifest _ ->
            decl in
      Tsig_modtype(id, newdecl) ::
      strengthen_sig (Env.add_modtype id decl env) rem p
      (* Need to add the module type in case it is manifest *)



(*************************************************************************)
(** {2 Non-dependent super types} *)

(* In nondep_supertype, env is only used for the type it assigns to id.
   Hence there is no need to keep env up-to-date by adding the bindings
   traversed. *)

exception Nondep_failure

type variance = Co | Contra | Strict

let nondep_supertype env mid mty =

  Env.set_current_env env;

  let isfree_typ typ =
    match typ.constr.tc_desc with
      TCfunction | TCtuple _ -> false
    | TCpath p -> Path.isfree mid p
  in

  let isfree_lvl = function
      Tlvl_principal _ -> false
    | Tlvl_path p -> 
	Path.isfree mid p or begin
	try
	  ignore (Env.find_level p env);
	  false
	with
	  Not_found -> true
	end
  in
  let not_isfree_lvl lvl = not (isfree_lvl lvl) in

  let nondep_level = function
      (Tlvl_path p) as lvl when isfree_lvl lvl  ->
	let lvd = Env.find_level p env in
	begin try
	  Level.Set.choose
	    (Level.Set.filter not_isfree_lvl
	       (Level.Set.inter lvd.lvd_lb_closed lvd.lvd_ub_closed))
	with Not_found -> raise Nondep_failure
	end
    | lvl -> lvl
  in

  let nondep_level_set = Level.Set.map nondep_level in

  let rec nondep_exception p =
    if Path.isfree mid p then begin
      let exn = Env.find_exception p env in
      match exn.exn_repr with
	Exn_manifest p' -> nondep_exception p'
      |	Exn_abstract -> raise Nondep_failure
    end
    else p    
  in
      

  let subst =
    { lb = nondep_level_set;
      ub = nondep_level_set;
      typ = (fun x -> x);
      label = nondep_exception
    } 
  in

  let nondep_value vald =
    try
      Value_description.copy ~subst ~expand:isfree_typ vald
    with
      Value_description.Copy_expand -> raise Nondep_failure
  in

  let nondep_constructor cstr =
    try
      Constructor_description.copy ~subst ~expand:isfree_typ cstr
    with
      Constructor_description.Copy_expand -> raise Nondep_failure
  in

  let nondep_label lbl =
    try
      Label_description.copy ~subst ~expand:isfree_typ lbl
    with
      Label_description.Copy_expand -> raise Nondep_failure
  in

  let nondep_type_decl is_covariant typd =
    let typd' = 
      try Type_declaration.copy ~subst ~expand:isfree_typ typd 
      with Type_declaration.Copy_expand when is_covariant ->
	{ typd with type_manifest = None }
    in
    try
      { typd' with 
	type_repr = match typd'.type_repr with
	  Type_abstract -> Type_abstract
	| Type_variant list ->
	    Type_variant (List.map (function name, cstr -> 
	      name, nondep_constructor cstr) list)
	| Type_record list ->
	    Type_record (List.map (function name, lbl -> 
	      name, nondep_label lbl) list)
      }	
    with Nondep_failure -> { typd with type_repr = Type_abstract }

  in

  let nondep_level_decl id is_covariant lvd =
    let lb = 
      Level.Set.fold (fun lvl lb ->
	try
	  Level.Set.add (nondep_level lvl) lb
	with
	  Nondep_failure when is_covariant -> lb
      ) lvd.lvd_lb Level.Set.empty
    and ub =
      Level.Set.fold (fun lvl ub ->
	try
	  Level.Set.add (nondep_level lvl) ub
	with
	  Nondep_failure when is_covariant -> ub
      ) lvd.lvd_ub Level.Set.empty
    in
    Transl_leveldecl.make_leveldecl env (Pident id) lb ub
  in

  let nondep_exception_decl exn =
    try
      let exn' = Exception_declaration.copy ~subst ~expand:isfree_typ exn in
      { exn' with
	exn_repr = match exn'.exn_repr with
	  Exn_abstract -> Exn_abstract
	| Exn_manifest p -> Exn_manifest (nondep_exception p)
      }	
    with
      Exception_declaration.Copy_expand -> raise Nondep_failure
  in

  let rec nondep_mty va mty =
    match mty with
      Tmty_ident p ->
        if Path.isfree mid p then
          nondep_mty va (Env.find_modtype_expansion p env)
        else mty
    | Tmty_signature sg ->
        Tmty_signature(nondep_sig va sg)
    | Tmty_functor(param, arg, pci, pcf, res) ->
        let var_inv =
          match va with Co -> Contra | Contra -> Co | Strict -> Strict in
        Tmty_functor(param, nondep_mty var_inv arg, 
		     nondep_level_set pci, 
		     nondep_level_set pcf, nondep_mty va res)

  and nondep_sig va = function
    [] -> []
  | item :: rem ->
      let rem' = nondep_sig va rem in
      match item with
        Tsig_value(id, d) ->
	  Tsig_value(id, nondep_value d) :: rem'
      | Tsig_type(id, d) ->
          Tsig_type(id, nondep_type_decl (va = Co) d) :: rem'
      |	Tsig_level(id, d) ->
	  Tsig_level(id, nondep_level_decl id (va = Co) d) :: rem'
      | Tsig_exception(id, d) ->
          Tsig_exception(id, nondep_exception_decl d) :: rem'
      | Tsig_module(id, mty) ->
          Tsig_module(id, nondep_mty va mty) :: rem'
      | Tsig_modtype(id, d) ->
          begin try
            Tsig_modtype(id, nondep_modtype_decl d) :: rem'
          with Nondep_failure ->
            match va with
              Co -> Tsig_modtype(id, Tmodtype_abstract) :: rem'
            | _  -> raise Nondep_failure
          end

  and nondep_modtype_decl = function
      Tmodtype_abstract -> Tmodtype_abstract
    | Tmodtype_manifest mty -> Tmodtype_manifest(nondep_mty Strict mty)

  in

  nondep_mty Co mty

