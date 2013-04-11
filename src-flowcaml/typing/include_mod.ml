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

(* $Id: include_mod.ml,v 1.4 2003/06/26 13:32:58 simonet Exp $ *)
(* Include_mod: Inclusion checks for the module language *)

open Format
open Misc
open Path
open Level
open Types
open Typedtree
open Print_types
open Include_error



(* *************************************************************************
   All functions "blah env x1 x2" check that x1 is included in x2,
   i.e. that x1 is the type of an implementation that fulfills the
   specification x2. If not, Error is raised with a backtrace of the error. 
 *)

(* Inclusion between value descriptions *)

let value_descriptions env subst id decl1 decl2 =
  let decl2 = Subst.value_description subst decl2 in
  Transl_value.included env id decl1 decl2;
  Tcoerce_none

(* Inclusion between type declarations *)

let type_declarations env subst id decl1 decl2 =
  let decl2 = Subst.type_declaration subst decl2 in
  Transl_typedecl.included env id decl1 decl2

(* Inclusion between level declarations *)

let level_declarations env subst id decl1 decl2 =
  let decl2 = Subst.level_declaration subst decl2 in
  Transl_leveldecl.compare env id decl1 decl2

(* -------------------------------------------------------------------------
   Inclusion between exception declarations *)

let exception_declarations env subst id decl1 decl2 =
  let decl2 = Subst.exception_declaration subst decl2 in
  Transl_exception.compare env id decl1 decl2



(* Expand a module type identifier when possible *)

exception Dont_match

let expand_module_path env path =
  try
    Env.find_modtype_expansion path env
  with Not_found ->
    raise Dont_match

(* Extract name, kind and ident from a signature item *)

type field_desc =
    Field_value of string
  | Field_type of string
  | Field_level of string
  | Field_exception of string
  | Field_module of string
  | Field_modtype of string

let item_ident_name = function
    Tsig_value(id, _) -> (id, Field_value(Ident.name id))
  | Tsig_type(id, _) -> (id, Field_type(Ident.name id))
  | Tsig_level(id,_) -> (id, Field_level(Ident.name id))
  | Tsig_exception(id, _) -> (id, Field_exception(Ident.name id))
  | Tsig_module(id, _) -> (id, Field_module(Ident.name id))
  | Tsig_modtype(id, _) -> (id, Field_modtype(Ident.name id))

(* Simplify a structure coercion *)

let simplify_structure_coercion cc =
  let pos = ref 0 in
  try
    List.iter
      (fun (n, c) ->
        if n <> !pos || c <> Tcoerce_none then raise Exit;
        incr pos)
      cc;
    Tcoerce_none
  with Exit ->
    Tcoerce_none

(* Inclusion between module types. 
   Return the restriction that transforms a value of the smaller type
   into a value of the bigger type. *)

let rec modtypes env subst mty1 mty2 =

  Env.set_current_env env;

  try
    try_modtypes env subst mty1 mty2
  with 
    Dont_match ->
      raise(Error[Module_types(mty1, Subst.modtype subst mty2)])
  | Error reasons ->
      raise(Error(Module_types(mty1, Subst.modtype subst mty2) :: reasons))

and try_modtypes env subst mty1 mty2 =

  Env.set_current_env env;

  match (mty1, mty2) with
    (_, Tmty_ident p2) ->
      try_modtypes2 env mty1 (Subst.modtype subst mty2)
  | (Tmty_ident p1, _) ->
      try_modtypes env subst (expand_module_path env p1) mty2
  | (Tmty_signature sig1, Tmty_signature sig2) ->
      signatures env subst sig1 sig2
  | (Tmty_functor(param1, arg1, pci1, pcf1, res1), 
     Tmty_functor(param2, arg2, pci2, pcf2, res2)) ->
      Transl_levelexpr.included_initialize env (pci1, pcf1) (pci2, pcf2);
      let cc_arg =
        modtypes env Subst.identity (Subst.modtype subst arg2) arg1
      in
      let cc_res =
        modtypes (Env.add_module param1 arg1 env)
          (Subst.add_module param2 (Pident param1) subst) res1 res2
      in
      begin match (cc_arg, cc_res) with
          (Tcoerce_none, Tcoerce_none) -> Tcoerce_none
        | _ -> Tcoerce_functor(cc_arg, cc_res)
      end
  | (_, _) ->
      raise Dont_match

and try_modtypes2 env mty1 mty2 =

  Env.set_current_env env;

  (* mty2 is an identifier *)
  match (mty1, mty2) with
    (Tmty_ident p1, Tmty_ident p2) when Path.same p1 p2 ->
      Tcoerce_none
  | (_, Tmty_ident p2) ->
      try_modtypes env Subst.identity mty1 (expand_module_path env p2)
  | (_, _) ->
      assert false

(* Inclusion between signatures *)

and signatures env subst sig1 sig2 =
  (* Environment used to check inclusion of components *)
  let new_env =
    Env.add_signature sig1 env in
  (* Build a table of the components of sig1, along with their positions.
     The table is indexed by kind and name of component *)
  let rec build_component_table pos tbl = function
      [] -> tbl
    | item :: rem ->
        let (id, name) = item_ident_name item in
        let nextpos =
          match item with
            (* Tsig_value(_,{val_kind = Val_prim _}) *)
          | Tsig_type(_,_)
	  | Tsig_level(_,_)
          | Tsig_modtype(_,_) -> pos
          | Tsig_value(_,_)
          | Tsig_exception(_,_)
          | Tsig_module(_,_) -> pos+1 in
        build_component_table nextpos
                              (Tbl.add name (id, item, pos) tbl) rem in
  let comps1 =
    build_component_table 0 Tbl.empty sig1 in
  (* Pair each component of sig2 with a component of sig1,
     identifying the names along the way.
     Return a coercion list indicating, for all run-time components
     of sig2, the position of the matching run-time components of sig1
     and the coercion to be applied to it. *)
  let rec pair_components subst paired unpaired = function
      [] ->
	begin
	  try
            begin match unpaired with
              [] -> signature_components new_env subst (List.rev paired)
            | _  -> raise(Error unpaired)
            end
	  with
	    Not_found -> assert false
	end
    | item2 :: rem ->
        let (id2, name2) = item_ident_name item2 in
        begin try
          let (id1, item1, pos1) = Tbl.find name2 comps1 in
          let new_subst =
            match item2 with
              Tsig_type _ ->
                Subst.add_type id2 (Pident id1) subst
            | Tsig_level _ ->
                Subst.add_level id2 (Pident id1) subst
	    | Tsig_exception _ ->
		Subst.add_exception id2 (Pident id1) subst
            | Tsig_module _ ->
                Subst.add_module id2 (Pident id1) subst
            | Tsig_modtype _ ->
                Subst.add_modtype id2 (Tmty_ident (Pident id1)) subst
            | Tsig_value _ ->
                subst
          in
          pair_components new_subst
            ((item1, item2, pos1) :: paired) unpaired rem
        with Not_found ->
          pair_components subst paired (Missing_field id2 :: unpaired) rem
        end in
  (* Do the pairing and checking, and return the final coercion *)
  simplify_structure_coercion(pair_components subst [] [] sig2)

(* Inclusion between signature components *)

and signature_components env subst sg =

  Env.set_current_env env;

  match sg with
    [] -> []
  | (Tsig_value(id1, valdecl1), Tsig_value(id2, valdecl2), pos) :: rem ->
      let cc = value_descriptions env subst id1 valdecl1 valdecl2 in
      (* begin match valdecl2.val_kind with
        Val_prim p -> signature_components env subst rem
      | _ -> *) (pos, cc) :: signature_components env subst rem
      (* end *)
  | (Tsig_type(id1, tydecl1), Tsig_type(id2, tydecl2), pos) :: rem ->
      type_declarations env subst id1 tydecl1 tydecl2;
      signature_components env subst rem
  | (Tsig_level(id1, lvd1), Tsig_level(id2, lvd2), pos) :: rem ->
      level_declarations env subst id1 lvd1 lvd2;
      signature_components env subst rem
  | (Tsig_exception(id1, excdecl1), Tsig_exception(id2, excdecl2), pos)
    :: rem ->
      exception_declarations env subst id1 excdecl1 excdecl2;
      (pos, Tcoerce_none) :: signature_components env subst rem
  | (Tsig_module(id1, mty1), Tsig_module(id2, mty2), pos) :: rem ->
      let cc =
        modtypes env subst (Mtype.strengthen env mty1 (Pident id1)) mty2 in
      (pos, cc) :: signature_components env subst rem
  | (Tsig_modtype(id1, info1), Tsig_modtype(id2, info2), pos) :: rem ->
      modtype_infos env subst id1 info1 info2;
      signature_components env subst rem
  | _ -> assert false

(* Inclusion between module type specifications *)

and modtype_infos env subst id info1 info2 =
  let info2 = Subst.modtype_declaration subst info2 in
  try
    match (info1, info2) with
      (Tmodtype_abstract, Tmodtype_abstract) -> ()
    | (Tmodtype_manifest mty1, Tmodtype_abstract) -> ()
    | (Tmodtype_manifest mty1, Tmodtype_manifest mty2) ->
        check_modtype_equiv env mty1 mty2
    | (Tmodtype_abstract, Tmodtype_manifest mty2) ->
        check_modtype_equiv env (Tmty_ident(Pident id)) mty2
  with Error reasons ->
    raise(Error(Modtype_infos(id, info1, info2) :: reasons))

and check_modtype_equiv env mty1 mty2 =
  match
    (modtypes env Subst.identity mty1 mty2,
     modtypes env Subst.identity mty2 mty1)
  with
    (Tcoerce_none, Tcoerce_none) -> ()
  | (_, _) -> raise(Error [Modtype_permutation])

(* Simplified inclusion check between module types *)

let check_modtype_inclusion env mty1 mty2 =
  try
    ignore(modtypes env Subst.identity mty1 mty2)
  with Error reasons ->
    raise Not_found

let _ = Env.check_modtype_inclusion := check_modtype_inclusion

(* Hide the substitution parameter to the outside world *)

let modtypes env mty1 mty2 = modtypes env Subst.identity mty1 mty2
let signatures env sig1 sig2 = signatures env Subst.identity sig1 sig2
let type_declarations env id decl1 decl2 =
  type_declarations env Subst.identity id decl1 decl2
let level_declarations env id decl1 decl2 =
  level_declarations env Subst.identity id decl1 decl2
