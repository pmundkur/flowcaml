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


(* $Id: type_mod.ml,v 1.11 2003/06/26 13:32:59 simonet Exp $ *)
(* Typemod: typechecking the module language *)

open Datastruct
open Parsetree
open Format
open Types
open Typedtree
open Print_types


let compare_scheme env ptys1 ptys2 =

  let translate ptys =
    let cset = Solver.cset () in
    let node_env = Transl_typeexpr.transl_variables cset
	(Transl_typeexpr.kind_scheme env ptys) in
    List.iter (Transl_typeexpr.transl_constraint cset env node_env)
      ptys.ptys_constraints;
    let vald =
      { val_cset = cset;
	val_context = Expr_context.empty;
	val_typ = Transl_typeexpr.transl_type cset env node_env ptys.ptys_type
      } 
    in

    ignore (Value_description.solve vald);
    vald

  in

  Include_mod.value_descriptions
    env
    Subst.identity
    (Ident.create "test")
    (translate ptys1) (translate ptys2)



(* *************************************************************************
   Reporting errors
 *)

type error =
    Unbound_module of Longident.t
  | Unbound_modtype of Longident.t
  | Cannot_apply of module_type
  | Not_included of Include_error.error list 
  | Cannot_eliminate_dependency of module_type
  | Signature_expected
  | Structure_expected of module_type
  | With_no_component of Longident.t
  | With_mismatch of Longident.t * Include_error.error list 
  | Repeated_name of string * string
(*  | Non_generalizable of type_expr *)
  | Non_generalizable_module of module_type
  | Illegal_with_level of Ident.t

exception Error of Location.t * error


let report_error ppf = function
  | Unbound_module lid -> 
      fprintf ppf "Unbound module %a" Longident.fprint lid
  | Unbound_modtype lid -> 
      fprintf ppf "Unbound module type %a" Longident.fprint lid
  | Cannot_apply mty ->
      fprintf ppf
        "@[This module is not a functor; it has type@ %a@]" 
	fprint_module_type mty

  | Not_included errs ->
      fprintf ppf
        "@[<v>Signature mismatch:@ %a@]" Include_error.report_error errs

  | Cannot_eliminate_dependency mty ->
      fprintf ppf
        "@[This functor has type@ %a@ \
           The parameter cannot be eliminated in the result type.@  \
           Please bind the argument to a module identifier.@]" 
	fprint_module_type mty

  | Signature_expected -> fprintf ppf "This module type is not a signature"
  | Structure_expected mty ->
      fprintf ppf
        "@[This module is not a structure; it has type@ %a" 
	fprint_module_type mty
	
  | With_no_component lid ->
      fprintf ppf
        "@[The signature constrained by `with' has no component named %a@]"
        Longident.fprint lid

  | With_mismatch(lid, explanation) ->
      fprintf ppf
        "@[<v>\
           @[In this `with' constraint, the new definition of %a@ \
             does not match its original definition@ \
             in the constrained signature:@]@ \
           %a@]"
        Longident.fprint lid Include_error.report_error explanation

  | Repeated_name(kind, name) ->
      fprintf ppf
        "@[Multiple definition of the %s name %s.@ \
           Names must be unique in a given structure or signature.@]" kind name
(*
  | Non_generalizable typ ->
      fprintf ppf
        "@[The type of this expression,@ %a,@ \
           contains type variables that cannot be generalized@]" type_scheme typ
*)
  | Non_generalizable_module mty ->
      fprintf ppf
        "@[The type of this module,@ %a,@ \
           contains type variables that cannot be generalized@]" 
	fprint_module_type mty

  | Illegal_with_level id ->
      fprintf ppf
	"@[The level %a is not abstract@]" Ident.fprint id



(* *************************************************************************
   Auxilliary functions
 *)

(* Extract a signature from a module type *)

let extract_sig env loc mty =
  match Mtype.scrape env mty with
    Tmty_signature sg -> sg
  | _ -> raise(Error(loc, Signature_expected))

let extract_sig_open env loc mty =
  match Mtype.scrape env mty with
    Tmty_signature sg -> sg
  | _ -> raise(Error(loc, Structure_expected mty))

(* Lookup the type of a module path *)

let type_module_path env loc lid =
  Env.lookup_module loc lid env

(* Merge one "with" constraint in a signature *)

let merge_constraint initial_env loc sg lid constr =
  let rec merge env sg namelist =
    Env.set_current_env env;
    match (sg, namelist, constr) with
      ([], _, _) ->
        raise(Error(loc, With_no_component lid))
    | (Tsig_type(id, decl) :: rem, [s], Pwith_type sdecl)
      when Ident.name id = s ->
        let newdecl = 
	  Transl_typedecl.translate_with_constraint initial_env sdecl
	in
        Include_mod.type_declarations env id newdecl decl;
        Tsig_type(id, newdecl) :: rem
    | (Tsig_level(id, decl) :: rem, [s], Pwith_level sdecl)
      when Ident.name id = s ->
	(* TEMPORARY : On pourrait améliorer *)
	let newdecl = Transl_leveldecl.translate initial_env id sdecl in
	Include_mod.level_declarations env id newdecl decl;
(*
	if not (Level.Set.is_empty decl.lvd_lb && Level.Set.is_empty decl.lvd_ub)
	then raise(Error(loc, Illegal_with_level id));
*)
	Tsig_level(id, newdecl) :: rem
    | (Tsig_module(id, mty) :: rem, [s], Pwith_module lid)
      when Ident.name id = s ->
        let (path, mty') = type_module_path initial_env loc lid in
        let newmty = Mtype.strengthen env mty' path in
        ignore(Include_mod.modtypes env newmty mty);
        Tsig_module(id, newmty) :: rem
    | (Tsig_module(id, mty) :: rem, s :: namelist, _) when Ident.name id = s ->
        let newsg = merge env (extract_sig env loc mty) namelist in
        Tsig_module(id, Tmty_signature newsg) :: rem
    | (item :: rem, _, _) ->
        item :: merge (Env.add_item item env) rem namelist in
  try
    merge initial_env sg (Longident.flatten lid)
  with Include_error.Error explanation ->
    raise(Error(loc, With_mismatch(lid, explanation)))



(* Extract a signature from a module type *)

let extract_sig env loc mty =
  match Mtype.scrape env mty with
    Tmty_signature sg -> sg
  | _ -> raise(Error(loc, Signature_expected))

let extract_sig_open env loc mty =
  match Mtype.scrape env mty with
    Tmty_signature sg -> sg
  | _ -> raise(Error(loc, Structure_expected mty))



(* Checking uniqueness of names in signatures and structures *)

let check cl loc set_ref name =
  if StringSet.mem name !set_ref
  then raise(Error(loc, Repeated_name(cl, name)))
  else set_ref := StringSet.add name !set_ref


let check_sig_item type_names module_names modtype_names loc = function
    Tsig_type(id, _) ->
      check "type" loc type_names (Ident.name id)
  | Tsig_module(id, _) ->
      check "module" loc module_names (Ident.name id)
  | Tsig_modtype(id, _) ->
      check "module type" loc modtype_names (Ident.name id)
  | _ -> ()



(* Try to convert a module expression to a module path. *)

exception Not_a_path

let rec path_of_module mexp =
  match mexp.mod_desc with
    Tmod_ident p -> p
(*  | Tmod_apply(funct, arg, coercion) ->
      Papply(path_of_module funct, path_of_module arg) *)
  | _ -> raise Not_a_path




(* *************************************************************************
   Checking and translating module type expressions
 *)

let rec transl_modtype env smty =
  match smty.pmty_desc with
    Pmty_ident lid ->
      begin try
        let (path, info) = Env.lookup_modtype smty.pmty_loc lid env in 
        Tmty_ident path
      with Not_found ->
        raise(Error(smty.pmty_loc, Unbound_modtype lid))
      end
  | Pmty_signature ssg ->
      Tmty_signature(transl_signature env ssg)

  | Pmty_functor(param, sarg, spci, spcf, sres, _) ->
      let arg = transl_modtype env sarg in
      let pci, pcf = Transl_levelexpr.transl_initialize env spci spcf in
      let (id, newenv) = Env.enter_module param arg env in
      let res = transl_modtype newenv sres in
      Tmty_functor(id, arg, pci, pcf, res)

  | Pmty_with(sbody, constraints) ->
      let body = transl_modtype env sbody in
      let init_sg = extract_sig env sbody.pmty_loc body in
      let final_sg =
        List.fold_left
          (fun sg pwth ->
            merge_constraint env smty.pmty_loc sg pwth.pwth_ident pwth.pwth_desc)
          init_sg constraints in
      Tmty_signature final_sg

  | Pmty_paren smty' ->
      transl_modtype env smty'


      
and transl_signature env sg =
  let type_names = ref StringSet.empty
  and level_names = ref StringSet.empty
  and exception_names = ref StringSet.empty
  and module_names = ref StringSet.empty
  and modtype_names = ref StringSet.empty in

  let rec transl_sig env sg =

    Env.set_current_env env;

    match sg with
      [] -> []
    | item :: srem ->

        match item.psig_desc with

        | Psig_value(name, sdesc) ->
            let id, desc, newenv = Transl_value.enter env name sdesc in
            let rem = transl_sig newenv srem in
            Tsig_value(id, desc) :: rem

        | Psig_type sdecls ->
            List.iter
              (fun (name, decl) -> check "type" item.psig_loc type_names name)
              sdecls;
            let decls = Transl_typedecl.translate env sdecls in
	    let newenv = 
	      List.fold_left (fun newenv (id, desc) ->
		Env.add_type id desc newenv
	      ) env decls
	    in
            let rem = transl_sig newenv srem in
            Misc.map_end (fun (id, info) -> Tsig_type(id, info)) decls rem

	| Psig_level (name, plvd) ->
	    check "level" item.psig_loc level_names name;
	    let id, desc, newenv = Transl_leveldecl.enter env name plvd in
	    let rem = transl_sig newenv srem in
	    Tsig_level(id, desc) :: rem

        | Psig_exception(name, pexn) ->
	    check "exception" item.psig_loc exception_names name;
            let id, desc, newenv = Transl_exception.enter env name pexn in
            let rem = transl_sig newenv srem in
            Tsig_exception(id, desc) :: rem

        | Psig_module(name, smty) ->
            check "module" item.psig_loc module_names name;
            let mty = transl_modtype env smty in
            let (id, newenv) = Env.enter_module name mty env in
            let rem = transl_sig newenv srem in
            Tsig_module(id, mty) :: rem

        | Psig_modtype(name, sinfo) ->
            check "module type" item.psig_loc modtype_names name;
            let info = transl_modtype_info env sinfo in
            let (id, newenv) = Env.enter_modtype name info env in
            let rem = transl_sig newenv srem in
            Tsig_modtype(id, info) :: rem

        | Psig_open lid ->
            let (path, mty) = type_module_path env item.psig_loc lid in
            let sg = extract_sig_open env item.psig_loc mty in
            let newenv = Env.open_signature path sg env in
            transl_sig newenv srem

        | Psig_include smty ->
            let mty = transl_modtype env smty in
            let sg = Subst.signature Subst.identity
                       (extract_sig env smty.pmty_loc mty) in
            List.iter
              (check_sig_item type_names module_names modtype_names
                              item.psig_loc)
              sg;
            let newenv = Env.add_signature sg env in
            let rem = transl_sig newenv srem in
            sg @ rem

  in
  transl_sig env sg

and transl_modtype_info env sinfo =
  match sinfo with
    Pmodtype_abstract ->
      Tmodtype_abstract
  | Pmodtype_manifest smty ->
      Tmodtype_manifest(transl_modtype env smty)



(* *************************************************************************
   Typing module expression
 *)

let check_pc loc pcf pci =
  if not (Solver.Lub.leq pcf pci) then begin
    Type_core.error (loc, Type_core.Initialize (pcf, pci))
  end



(* Extract the list of "value" identifiers bound by a signature.
   "Value" identifiers are identifiers for signature components that
   correspond to a run-time value: values, exceptions, modules, classes.
   Note: manifest primitives do not correspond to a run-time value! *)

let rec bound_value_identifiers = function
    [] -> []
  | Tsig_value(id, (* TEMPORARY {val_kind = Val_reg} *) _) :: rem ->
      id :: bound_value_identifiers rem
  | Tsig_exception(id, decl) :: rem -> id :: bound_value_identifiers rem
  | Tsig_module(id, mty) :: rem -> id :: bound_value_identifiers rem
  | _ :: rem -> bound_value_identifiers rem




let rec type_module env pc smod =

  Env.set_current_env env;

  match smod.pmod_desc with
    Pmod_ident lid ->
      let (path, mty) = type_module_path env smod.pmod_loc lid in
        { mod_desc = Tmod_ident path;
	  mod_type = Mtype.strengthen env mty path;
	  mod_env = env;
	  mod_loc = smod.pmod_loc
	}, Solver.Ub.top, Solver.Lb.bottom

  | Pmod_structure sstr ->
      let str, sg, pci, pcf, finalenv = type_structure env pc sstr in
      { mod_desc = Tmod_structure str;
        mod_type = Tmty_signature sg;
        mod_env = env;
        mod_loc = smod.pmod_loc 
      }, pci, pcf

  | Pmod_functor(name, smty, sbody) ->
      let mty = transl_modtype env smty in
      let (id, newenv) = Env.enter_module name mty env in
      let body, pci, pcf = type_module newenv pc sbody in
      { mod_desc = Tmod_functor(id, mty, body);
        mod_type = Tmty_functor(id, mty, pci, pcf, body.mod_type);
        mod_env = env;
        mod_loc = smod.pmod_loc
      }, Solver.Ub.top, Solver.Lb.bottom

  | Pmod_apply(sfunct, sarg) ->
      let arg, pci_arg, pcf_arg = type_module env pc sarg in
      let funct, pci_funct, pcf_funct = 
	type_module env (Solver.Lb.union pcf_arg pc) sfunct 
      in
      begin match Mtype.scrape env funct.mod_type with
        Tmty_functor(param, mty_param, pci, pcf, mty_res) as mty_functor ->
	  check_pc smod.pmod_loc pc pci;
          let coercion =
            try
              Include_mod.modtypes env arg.mod_type mty_param
            with Include_error.Error msg ->
              raise(Error(sarg.pmod_loc, Not_included msg)) in
          let mty_appl =
            try
              let path = path_of_module arg in
              Subst.modtype (Subst.add_module param path Subst.identity)
                mty_res
            with Not_a_path ->
              try
                Mtype.nondep_supertype
                  (Env.add_module param arg.mod_type env) param mty_res
              with 
		Mtype.Nondep_failure ->
                  raise(Error(smod.pmod_loc,
                              Cannot_eliminate_dependency mty_functor)) 
	      |	Not_found -> assert false
	  in
          { mod_desc = Tmod_apply(funct, arg, coercion);
            mod_type = mty_appl;
            mod_env = env;
            mod_loc = smod.pmod_loc
	  },
	  Solver.Ub.inter pci (Solver.Ub.inter pci_funct pci_arg),
	  Solver.Lb.union pcf (Solver.Lb.union pcf_funct pcf_arg)

      | _ ->
          raise(Error(sfunct.pmod_loc, Cannot_apply funct.mod_type))
      end        

  | Pmod_constraint(sarg, smty) ->
      let arg, pci, pcf = type_module env pc sarg in
      let mty = transl_modtype env smty in
      let coercion =
        try
          Include_mod.modtypes env arg.mod_type mty
        with Include_error.Error msg ->
          raise(Error(sarg.pmod_loc, Not_included msg)) in
      { mod_desc = Tmod_constraint(arg, mty, coercion);
        mod_type = mty;
        mod_env = env;
        mod_loc = smod.pmod_loc
      }, pci, pcf

  | Pmod_paren smod' ->
      type_module env pc smod'



and type_structure env pc sstr =
  let type_names = ref StringSet.empty
  and level_names = ref StringSet.empty
  and exception_names = ref StringSet.empty
  and module_names = ref StringSet.empty
  and modtype_names = ref StringSet.empty
  in

  let rec type_struct env pc sstr =

    Env.set_current_env env;

    match sstr with

      [] -> ([], [], Solver.Ub.top, Solver.Lb.bottom, env)

    | { pstr_desc = Pstr_eval pexp; pstr_loc = loc } :: srem ->

	let exp, pci, pcf = Type_core.type_eval env loc pexp in

	check_pc loc pc pci;

        let str_rem, sig_rem, pci_rem, pcf_rem, final_env = 
	  type_struct env (Solver.Lb.union pc pcf) srem 
	in
	(Tstr_eval exp :: str_rem, 
	 sig_rem, 
	 Solver.Ub.inter pci pci_rem, Solver.Lb.union pcf pcf_rem,
	 final_env)



  | { pstr_desc = Pstr_value (rec_flag, ppat_pexp_list); pstr_loc = loc }
    :: srem  ->

      let pat_exp_list, signature, newenv, pci, pcf =
	Type_core.type_let_toplevel env loc rec_flag ppat_pexp_list
      in

      check_pc loc pc pci;

      let str_rem, sig_rem, pci_rem, pcf_rem, final_env = 
	type_struct newenv (Solver.Lb.union pc pcf) srem
      in
      (Tstr_value(rec_flag, pat_exp_list) :: str_rem,
       signature @ sig_rem,
       Solver.Ub.inter pci pci_rem, Solver.Lb.union pcf pcf_rem,
       final_env)



  | { pstr_desc = Pstr_primitive (name, sdesc); pstr_loc = loc} 
    :: srem ->

      let id, desc, newenv = Transl_value.enter env name sdesc in
      let str_rem, sig_rem, pci_rem, pcf_rem, final_env =
	type_struct newenv pc srem
      in
      (Tstr_primitive(id, desc) :: str_rem,
       Tsig_value (id, desc) :: sig_rem, 
       pci_rem, pcf_rem,
       final_env)



  | { pstr_desc = Pstr_type sdecls; pstr_loc = loc} 
    :: srem ->

      List.iter
        (fun (name, decl) -> check "type" loc type_names name)
        sdecls;

      let decls, newenv = Transl_typedecl.enter env sdecls in

      let str_rem, sig_rem, pci_rem, pcf_rem, final_env = 
	type_struct newenv pc srem
      in
      (Tstr_type decls :: str_rem,
       Misc.map_end (fun (id, info) -> Tsig_type(id, info)) decls sig_rem,
       pci_rem, pcf_rem,
       final_env)



  | { pstr_desc = Pstr_level (name, plvd); pstr_loc = loc }
    :: srem ->

      check "level" loc level_names name;

      let id, desc, newenv = Transl_leveldecl.enter env name plvd in

      let str_rem, sig_rem, pci_rem, pcf_rem, final_env = 
	type_struct newenv pc srem
      in

      (Tstr_level(id, desc) :: str_rem,
       Tsig_level(id, desc) :: sig_rem,
       pci_rem, pcf_rem,
       final_env)



  | { pstr_desc = Pstr_exception (name, sarg); pstr_loc = loc }
    :: srem ->

      check "exception" loc exception_names name;

      let id, desc, newenv = Transl_exception.enter env name sarg in

      let str_rem, sig_rem, pci_rem, pcf_rem, final_env = 
	type_struct newenv pc srem 
      in

      (Tstr_exception(id, desc) :: str_rem,
       Tsig_exception(id, desc) :: sig_rem,
       pci_rem, pcf_rem,
       final_env)



  | { pstr_desc = Pstr_module (name, smodl); pstr_loc = loc }
    :: srem ->
      check "module" loc module_names name;

      let modl, pci, pcf = type_module env pc smodl in
      let (id, newenv) = Env.enter_module name modl.mod_type env in

      let str_rem, sig_rem, pci_rem, pcf_rem, final_env = 
	type_struct newenv (Solver.Lb.union pc pcf) srem
      in
      (Tstr_module(id, modl) :: str_rem, 
       Tsig_module(id, modl.mod_type) :: sig_rem, 
       Solver.Ub.inter pci pci_rem, Solver.Lb.union pcf pcf_rem,
       final_env)


  | {pstr_desc = Pstr_modtype(name, smty); pstr_loc = loc}
    :: srem ->
      check "module type" loc modtype_names name;

      let mty = transl_modtype env smty in
      let (id, newenv) = Env.enter_modtype name (Tmodtype_manifest mty) env in

      let str_rem, sig_rem, pci_rem, pcf_rem, final_env =
	type_struct newenv pc srem
      in
      (Tstr_modtype(id, mty) :: str_rem, 
       Tsig_modtype(id, Tmodtype_manifest mty) :: sig_rem, 
       pci_rem, pcf_rem,
       final_env)

  | { pstr_desc = Pstr_open lid; pstr_loc = loc } :: srem ->

      let (path, mty) = type_module_path env loc lid in
      let sg = extract_sig_open env loc mty in
      type_struct (Env.open_signature path sg env) pc srem


  | { pstr_desc = Pstr_include smodl; pstr_loc = loc } :: srem -> 
      let modl, pci, pcf = type_module env pc smodl in
        (* Rename all identifiers bound by this signature to avoid clashes *)
      let sg = Subst.signature Subst.identity
          (extract_sig_open env smodl.pmod_loc modl.mod_type) in
      List.iter
        (check_sig_item type_names module_names modtype_names loc) sg;
      let new_env = Env.add_signature sg env in
      let str_rem, sig_rem, pci_rem, pcf_rem, final_env = 
	type_struct new_env (Solver.Lb.union pc pcf) srem
      in
      (Tstr_include (modl, bound_value_identifiers sg) :: str_rem,
       sg @ sig_rem,
       Solver.Ub.inter pci pci_rem, Solver.Lb.union pcf pcf_rem,
       final_env)

  in

  type_struct env pc sstr
