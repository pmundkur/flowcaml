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

(* $Id: depend.ml,v 1.7 2003/06/26 13:32:54 simonet Exp $ *)
(* Depend *)

open Format
open Datastruct
open Location
open Longident
open Parsetree


(* Collect free module identifiers in the a.s.t. *)

let free_structure_names = ref StringSet.empty

let rec addmodule bv lid =
  match lid with
    Lident s ->
      if not (StringSet.mem s bv)
      then free_structure_names := StringSet.add s !free_structure_names
  | Ldot(l, s) -> addmodule bv l
  | Lapply(l1, l2) -> addmodule bv l1; addmodule bv l2

let add bv lid =
  match lid with
    Ldot(l, s) -> addmodule bv l
  | _ -> ()

let add_level bv plvl =
  match plvl.plvl_desc with
    Plvl_ident lid -> add bv lid
  | Plvl_principal _ -> ()

let add_bound bv list =
  List.iter (add_level bv) list

let rec add_type bv ty =
  match ty.ptyp_desc with
    Ptyp_var v -> ()
  | Ptyp_arrow(t1, t2, t3, t4, t5) ->
      add_type bv t1; add_type bv t2; add_type bv t3;
      add_type bv t4; add_type bv t5
  | Ptyp_tuple tl -> List.iter (add_type bv) tl
  | Ptyp_constr(c, tl) -> add bv c; List.iter (add_type bv) tl
  | Ptyp_bounds (_, lb, ub) -> add_bound bv lb; add_bound bv ub
  | Ptyp_row (lid, t1, t2) -> add bv lid; add_type bv t1; add_type bv t2
  | Ptyp_arrow_abbrev(tl, t1, t2, t3) ->
      List.iter (add_type bv) tl; add_type bv t1; add_type bv t2; add_type bv t3
  | Ptyp_paren t -> add_type bv t

let add_constraint bv ptyc =
  match ptyc.ptyc_desc with
    Ptyc_ssk list -> List.iter (add_type bv) list
  | Ptyc_leq (lhs, rhs) ->
      List.iter (fun (_, ptyp) -> add_type bv ptyp) lhs;
      List.iter (fun (_, ptyp) -> add_type bv ptyp) rhs

let add_scheme bv ptys =
  add_type bv ptys.ptys_type;
  List.iter (add_constraint bv) ptys.ptys_constraints

let add_opt add_fn bv = function
    None -> ()
  | Some x -> add_fn bv x

let add_opt1 add_fn bv = function
    None -> ()
  | Some (x, _) -> add_fn bv x

let add_type_declaration bv td =
  List.iter
    (fun (ty1, ty2, _) -> add_type bv ty1; add_type bv ty2)
    td.ptype_cstrs;
  add_opt add_type bv td.ptype_manifest;
  match td.ptype_repr with
    Ptype_abstract -> ()
  | Ptype_variant (cstrs, opt_lvl) ->
      List.iter (fun (c, args) -> List.iter (add_type bv) args) cstrs;
      add_opt1 add_type bv opt_lvl
  | Ptype_record (lbls, opt_lvl) ->
      List.iter (fun (l, mut, ty) -> add_type bv ty) lbls;
      add_opt1 add_type bv opt_lvl

let add_level_declaration bv ld =
  List.iter (add_level bv) ld.plvd_lb;
  List.iter (add_level bv) ld.plvd_ub

let add_exception_declaration bv ed =
  List.iter (add_type bv) (snd ed.pexn_type);
  add_opt (fun bv (lid, _) -> add bv lid) bv ed.pexn_manifest

let rec add_pattern bv pat =
  match pat.ppat_desc with
    Ppat_any -> ()
  | Ppat_var _ -> ()
  | Ppat_alias(p, _) -> add_pattern bv p
  | Ppat_constant _ -> ()
  | Ppat_tuple pl -> List.iter (add_pattern bv) pl
  | Ppat_construct(c, op, _) -> add bv c; add_opt add_pattern bv op
  | Ppat_record pl ->
      List.iter (fun (lbl, p) -> add bv lbl; add_pattern bv p) pl
  | Ppat_array pl -> List.iter (add_pattern bv) pl
  | Ppat_or(p1, p2) -> add_pattern bv p1; add_pattern bv p2
  | Ppat_constraint(p, ty) -> add_pattern bv p; add_scheme bv ty
  | Ppat_paren p -> add_pattern bv p

let add_try_pat bv ptry =
  match ptry.ptry_desc with
    Ptry_any -> ()
  | Ptry_list list ->
      List.iter (function ptryi ->
	add bv ptryi.ptryi_exception;
	add_opt add_pattern bv ptryi.ptryi_arg
      ) list

let rec add_expr bv exp =
  match exp.pexp_desc with
    Pexp_ident l -> add bv l
  | Pexp_constant _ -> ()
  | Pexp_let(_, pel, e) -> add_pat_expr_list bv pel; add_expr bv e
  | Pexp_function pel ->
      add_pat_when_expr_list bv pel
  | Pexp_apply(e, el) ->
      add_expr bv e; List.iter (fun e -> add_expr bv e) el
  | Pexp_match(e, pel) -> add_expr bv e; add_pat_when_expr_list bv pel
  | Pexp_raise(lid, opte) -> add bv lid; add_opt add_expr bv opte
  | Pexp_try(e, pel) -> add_expr bv e; 
      List.iter (fun (p, e, _) -> add_try_pat bv p; add_expr bv e) pel
  | Pexp_finally(e1, e2) -> add_expr bv e1; add_expr bv e2
  | Pexp_tuple el -> List.iter (add_expr bv) el
  | Pexp_construct(c, opte, _) -> add bv c; add_opt add_expr bv opte
  | Pexp_record(lblel, opte) ->
      List.iter (fun (lbl, e) -> add bv lbl; add_expr bv e) lblel;
      add_opt add_expr bv opte
  | Pexp_field(e, fld) -> add_expr bv e; add bv fld
  | Pexp_setfield(e1, fld, e2) -> add_expr bv e1; add bv fld; add_expr bv e2
  | Pexp_array el -> List.iter (add_expr bv) el
  | Pexp_ifthenelse(e1, e2, opte3) ->
      add_expr bv e1; add_expr bv e2; add_opt add_expr bv opte3
  | Pexp_sequence(e1, e2) -> add_expr bv e1; add_expr bv e2
  | Pexp_while(e1, e2) -> add_expr bv e1; add_expr bv e2
  | Pexp_for(_, e1, e2, _, e3) ->
      add_expr bv e1; add_expr bv e2; add_expr bv e3
  | Pexp_constraint(e1, ty2) ->
      add_expr bv e1;
      add_scheme bv ty2
  | Pexp_assert (e) -> add_expr bv e
  | Pexp_assertfalse -> ()
  | Pexp_paren e -> add_expr bv e

and add_pat_expr_list bv pel =
  List.iter (fun (p, e) -> add_pattern bv p; add_expr bv e) pel

and add_pat_when_expr_list bv pel =
  List.iter (fun (p, opt_e, e) -> 
    add_pattern bv p; 
    begin match opt_e with None -> () | Some e' -> add_expr bv e' end;
    add_expr bv e
  ) pel

and add_modtype bv mty =
  match mty.pmty_desc with
    Pmty_ident l -> add bv l
  | Pmty_signature s -> add_signature bv s
  | Pmty_functor(id, mty1, pci, pcf, mty2, _) ->
      add_modtype bv mty1; add_modtype (StringSet.add id bv) mty2;
      add_bound bv pci; add_bound bv pcf
  | Pmty_with(mty, cstrl) ->
      add_modtype bv mty;
      List.iter
        (function { pwth_desc = Pwith_type td } -> add_type_declaration bv td
       	        | { pwth_desc = Pwith_level ld } -> add_level_declaration bv ld
                | { pwth_desc = Pwith_module lid } -> addmodule bv lid)
        cstrl
  | Pmty_paren mty ->
      add_modtype bv mty

and add_signature bv = function
    [] -> ()
  | item :: rem -> add_signature (add_sig_item bv item) rem

and add_sig_item bv item =
  match item.psig_desc with
    Psig_value(id, vd) ->
      add_scheme bv vd.pval_type; bv
  | Psig_type dcls ->
      List.iter (fun (id, td) -> add_type_declaration bv td) dcls; bv
  | Psig_level(id, decl) ->
      add_level_declaration bv decl; bv
  | Psig_exception(id, decl) ->
      add_exception_declaration bv decl; bv
  | Psig_module(id, mty) ->
      add_modtype bv mty; StringSet.add id bv
  | Psig_modtype(id, mtyd) ->
      begin match mtyd with
        Pmodtype_abstract -> ()
      | Pmodtype_manifest mty -> add_modtype bv mty
      end;
      bv
  | Psig_open lid ->
      addmodule bv lid; bv
  | Psig_include mty ->
      add_modtype bv mty; bv

and add_module bv modl =
  match modl.pmod_desc with
    Pmod_ident l -> addmodule bv l
  | Pmod_structure s -> ignore (add_structure bv s)
  | Pmod_functor(id, mty, modl) ->
      add_modtype bv mty;
      add_module (StringSet.add id bv) modl
  | Pmod_apply(mod1, mod2) ->
      add_module bv mod1; add_module bv mod2
  | Pmod_constraint(modl, mty) ->
      add_module bv modl; add_modtype bv mty
  | Pmod_paren modl ->
      add_module bv modl

and add_structure bv item_list =
  List.fold_left add_struct_item bv item_list 

and add_struct_item bv item =
  match item.pstr_desc with
    Pstr_eval e ->
      add_expr bv e; bv
  | Pstr_value(id, pel) ->
      add_pat_expr_list bv pel; bv
  | Pstr_primitive(id, vd) ->
      add_scheme bv vd.pval_type; bv
  | Pstr_type dcls ->
      List.iter (fun (id, td) -> add_type_declaration bv td) dcls; bv
  | Pstr_level(id, decl) ->
      add_level_declaration bv decl; bv
  | Pstr_exception(id, decl) ->
      add_exception_declaration bv decl; bv
  | Pstr_module(id, modl) ->
      add_module bv modl; StringSet.add id bv
  | Pstr_modtype(id, mty) ->
      add_modtype bv mty; bv
  | Pstr_open l ->
      addmodule bv l; bv
  | Pstr_include modl ->
      add_module bv modl; bv

and add_use_file bv top_phrs =
  ignore (List.fold_left add_top_phrase bv top_phrs)

and add_top_phrase bv = function
  | Ptop_def str -> add_structure bv str
  | Ptop_flow _
  | Ptop_dir (_, _) -> bv

let add_implementation bv pimp =
  ignore (add_structure bv pimp.pimp_structure)

let add_interface bv pint =
  ignore (add_signature bv pint.pint_signature)
