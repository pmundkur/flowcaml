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

(* $Id: transl_typeexpr.ml,v 1.5 2003/06/26 13:32:58 simonet Exp $ *)
(* Transl_type: translating core type expressions into internal types. *)

open Format
open Dalton_aux
open Fkind
open Parsetree
open Type_constructor
open Level
open Types
open Transl_levelexpr



let ( < ) nd1 nd2 =
  Solver.strong_leq nd1 nd2

let ( <& ) nd1 nd2 =
  Solver.weak_leq (Solver.Nd nd1) (Solver.Nd nd2)

let (<|) nd1 nd2 =
  Solver.weak_leq (Solver.Nd nd1) (Solver.Nd nd2)

let lower_bound lb nd =
  Solver.lower_bound lb (Solver.Nd nd)

let upper_bound nd ub =
  Solver.upper_bound (Solver.Nd nd) ub



(* *************************************************************************
   Reporting errors
 *)

type error =
    Unbound_type_variable of string
  | Unbound_constructor of Longident.t
  | Type_arity_mismatch of Longident.t * int * int
  | Kind_clash of Ukind.t * Ukind.t (* expected, provided *)
  | Underspecified_kind of string
  | Underspecified_domain of string
  | Illegal_arrow_abbrev
  | Illegal_bound_in_exception

exception Error of Location.t * error

let error (loc, err) = raise (Error (loc, err))


let fprint_variable ppf name =
  if String.length name > 0 && name.[0] = '%' then
    fprintf ppf "an anonymous type variable"
  else
    fprintf ppf "'%s" name

let report_error ppf = function
    Unbound_type_variable name 
    when String.length name > 0 && name.[0] = '%' ->
      fprintf ppf "Anonymous type variables are not allowed in type declarations"
  | Unbound_type_variable name ->
      fprintf ppf "The type variable '%s is unbound in this declaration" 
	name
  | Unbound_constructor lid ->
      fprintf ppf "Unbound constructor %a" Longident.fprint lid
  | Type_arity_mismatch(lid, expected, provided) ->
      fprintf ppf
       "@[The type constructor %a@ expects %i argument(s),@ \
        but is here applied to %i argument(s)@]"
       Longident.fprint lid expected provided
  | Kind_clash (expected, provided) ->
      fprintf ppf
	"@[This type expression has kind %a@ but is here used with kind@ \
        %a@]" Ukind.fprint provided Ukind.fprint expected
  | Underspecified_kind name ->
      fprintf ppf
	"The kind of %a cannot be inferred in this scheme" 
	fprint_variable name
  | Underspecified_domain name ->
      fprintf ppf
	"The domaine of %a cannot be inferred in this scheme" 
	fprint_variable name
  | Illegal_arrow_abbrev ->
      fprintf ppf "Anonymous type variables are not allowed in type declarations"
  | Illegal_bound_in_exception ->
      fprintf ppf "This abbreviation cannot occur in an exception declaration"


(* *************************************************************************
   Kinding core type expressions
 *)

let kind_level env plvl =
  match plvl.plvl_desc with
    Plvl_principal _ -> ()
  | Plvl_ident lid ->
      ignore (Env.lookup_level plvl.plvl_loc lid env)

let kind_bound env list = 
  List.iter (kind_level env) list

(* [kind_type ext env kind_env constr_env typ] infers the kind of a type
   expression.
   - [ext] is a boolean indicating wether [kind_env] is extensible
   - [env] is the current environment (of type [Env.t])
   - [kind_env] is a reference on an association list giving the
     kind of each free variable of the type.
   - [constr_env] is an association list giving the paramaters kinds of
     the the new type constructors.
   - [ptyp] is the expression to be kind-checked.
 *)

let rec kind_type ?(exn=false) ext env kind_env constr_env ptyp =

  match ptyp.ptyp_desc with

    Ptyp_var name ->
      begin try List.assoc name !kind_env 
      with Not_found ->
	if not ext then error (ptyp.ptyp_loc, Unbound_type_variable name);
	let k = Ukind.create Ukind.Var in
	kind_env := (name, k) :: !kind_env;
	k
      end

  | Ptyp_arrow (t1, t2, t3, t4, t5) ->
      kind_type_expect ~exn ext env kind_env constr_env Ukind.typ t1;
      kind_type_expect ~exn ext env kind_env constr_env Ukind.level t2;
      kind_type_expect ~exn ext env kind_env constr_env Ukind.row t3;
      kind_type_expect ~exn ext env kind_env constr_env Ukind.typ t4;
      kind_type_expect ~exn ext env kind_env constr_env Ukind.level t5;
      Ukind.typ

  | Ptyp_arrow_abbrev (args, pc, row, res) ->
      if not ext then error (ptyp.ptyp_loc, Illegal_arrow_abbrev);
      List.iter (kind_type_expect ~exn ext env kind_env constr_env Ukind.typ) 
	args;
      kind_type_expect ~exn ext env kind_env constr_env Ukind.level pc;
      kind_type_expect ~exn ext env kind_env constr_env Ukind.row row;
      kind_type_expect ~exn ext env kind_env constr_env Ukind.typ res;
      Ukind.typ

  | Ptyp_tuple list ->
      List.iter (kind_type_expect ~exn ext env kind_env constr_env Ukind.typ) 
	list;
      Ukind.typ      
	
  | Ptyp_constr (lid, ptyp_list) ->

      let expected =
	try 
	  List.assoc (Longident.lident lid) constr_env
	with
	  Not_found -> List.map Ukind.unfreeze 
	      (snd (Env.lookup_type ptyp.ptyp_loc lid env)).type_kinds
      in

      begin 
	try 
	  List.iter2 (kind_type_expect ~exn ext env kind_env constr_env) 
	    expected ptyp_list
	with
	  Invalid_argument "List.iter2" ->
	    error (ptyp.ptyp_loc, Type_arity_mismatch 
		     (lid, List.length expected, List.length ptyp_list))
      end;
      Ukind.typ

  | Ptyp_bounds (row_flag, plb, pub) ->
      if exn && plb <> pub then 
	error (ptyp.ptyp_loc, Illegal_bound_in_exception);
      kind_bound env plb;
      kind_bound env pub;
      if row_flag then 
	Ukind.create (Ukind.Urow (Uset.create (Uset.Var Cset.empty)))
      else
	Ukind.level      

  | Ptyp_row (lid, t1, t2) ->
      let exn_path, _ = Env.lookup_exception ptyp.ptyp_loc lid env in
      kind_type_expect ~exn ext env kind_env constr_env Ukind.level t1;
      let set = Uset.create (Uset.Var (Cset.singleton exn_path)) in
      let set2 = Uset.create (Uset.Element (exn_path, set)) in
      kind_type_expect ~exn ext env kind_env constr_env
	(Ukind.create (Ukind.Urow set2)) t2;
      Ukind.create (Ukind.Urow set)

  | Ptyp_paren ptyp' ->
      kind_type ~exn ext env kind_env constr_env ptyp'


and kind_type_expect  ?(exn=false) 
    ext env kind_env constr_env expected_kind ptyp =

  let kind = kind_type ~exn ext env kind_env constr_env ptyp in    

  try 
    Ukind.unify kind expected_kind
  with
    Ukind.Unify | Uset.Unify ->
      error (ptyp.ptyp_loc, Kind_clash (expected_kind, kind))



(* [kind_handside_expect] kinds an hand-side.  It accepts the following
   arguments:
   - the location [loc] of the whole constraint
   - the variable environment [venv]
   - the expected kind [expected_kind]
   - and the hand-side
 *)

let kind_handside_expect loc env kind_env expected_kind (destr, typ) =

  let typ_expected_kind =
    if not destr then expected_kind
    else begin
      try
	Ukind.unify Ukind.level expected_kind;
	Ukind.create Ukind.Var
      with Ukind.Unify | Uset.Unify ->
	error (typ.ptyp_loc, Kind_clash (expected_kind, Ukind.level))
    end
  in

  kind_type_expect true env kind_env [] typ_expected_kind typ



let level loc kind =
  try
    Ukind.unify Ukind.level kind
  with
    Ukind.Unify | Uset.Unify ->
      error (loc, Kind_clash (kind, Ukind.level))


(* [kind_constraint] kinds a constraint. *)

let kind_constraint env kind_env ptyc =

  match ptyc.ptyc_desc with

    Ptyc_ssk list ->
      List.iter (kind_type_expect true env kind_env [] Ukind.typ) list

  | Ptyc_leq (lhs_list, rhs_list) ->
      let kind = Ukind.create Ukind.Var in

      List.iter (function 
	  NoDestr, ptyp ->
	    kind_type_expect true env kind_env [] kind ptyp
	| (Destr | SkDestr), ptyp ->
	    level ptyc.ptyc_loc kind;
	    ignore (kind_type true env kind_env [] ptyp)
      ) lhs_list;

      List.iter (function 
	  NoDestr, ptyp ->
	    kind_type_expect true env kind_env [] kind ptyp
	| (Destr | SkDestr), ptyp -> 
	    level ptyc.ptyc_loc kind;
	    ignore (kind_type true env kind_env [] ptyp)

      ) rhs_list



(* [kind_scheme] kinds a type scheme.  It returns an association list giving
   the inferred kind of each free variable.
 *)

let kind_scheme env tys =

  let kind_env = ref [] in

  kind_type_expect true env kind_env [] Ukind.typ tys.ptys_type;
  List.iter (kind_constraint env kind_env) tys.ptys_constraints;

  List.map (function name, kind ->
    try name, Ukind.freeze kind
    with
      Ukind.Freeze -> error (tys.ptys_loc, Underspecified_kind name)
    | Uset.Freeze -> error (tys.ptys_loc, Underspecified_domain name)
  ) (! kind_env)



(* *************************************************************************
   Translating core type expressions into internal types.
 *)

let transl_variables cset type_variables =
  List.map (function name, kind ->
    name, Solver.variable cset (Fkind.daltonize kind)
  ) type_variables



let rec transl_type cset env node_env ptyp =

  match ptyp.ptyp_desc with

    Ptyp_var name ->
      List.assoc name node_env

  | Ptyp_arrow (ptyp1, ptyp2, ptyp3, ptyp4, ptyp5) ->
      Solver.typ cset begin
	Predef.funct (transl_type cset env node_env ptyp1)
	  (transl_type cset env node_env ptyp2)
	  (transl_type cset env node_env ptyp3)
	  (transl_type cset env node_env ptyp4)
	  (transl_type cset env node_env ptyp5)
      end

  | Ptyp_arrow_abbrev (ptyp_args, ptyp_pc, ptyp_row, ptyp_res) ->
      let res = transl_type cset env node_env ptyp_res
      and row = transl_type cset env node_env ptyp_row
      and pc = transl_type cset env node_env ptyp_pc in
      let typ, pc_1, row_1 =
	List.fold_right (fun ptyp_arg (typ, pc_i', row_i') ->
	  let arg = transl_type cset env node_env ptyp_arg in
	  let pc_i = Solver.variable cset Katom
	  and row_i = Solver.variable cset (Krow Katom)
	  and lvl_i = Solver.variable cset Katom in
	  pc_i < pc_i';
	  row_i < row_i';
	  row_i <& pc_i';
	  lvl_i <| res;
	  lvl_i < pc_i;
	  (Solver.typ cset 
	     (Predef.funct arg pc_i row_i typ lvl_i), pc_i, row_i)
	) ptyp_args (res, Solver.variable cset Katom, row)
      in
      pc < pc_1;
      typ

  | Ptyp_tuple list ->
      Solver.typ cset
	(Predef.tuple (List.map (transl_type cset env node_env) list))

  | Ptyp_constr (lid, ptyp_list) ->
      let path, decl = Env.lookup_type ptyp.ptyp_loc lid env in
      Solver.typ cset
      { Solver.constr = { tc_desc = TCpath path;
			  tc_kinds = decl.type_kinds;
			  tc_prop = decl.type_prop;
			  tc_fun = decl.type_fun
			} ;
	Solver.sons = List.map (transl_type cset env node_env) ptyp_list
      }	

  | Ptyp_bounds (row_flag, plb, pub) ->
      let lvl = Solver.variable cset (if row_flag then Krow Katom else Katom) in
      lower_bound (transl_level_set env plb) lvl;
      upper_bound lvl (transl_level_set env pub);
      lvl

  | Ptyp_row (lid, ptyp1, ptyp2) ->
      let exn_path, _ = Env.lookup_exception ptyp.ptyp_loc lid env in
      Solver.row cset (exn_path, 
			      transl_type cset env node_env ptyp1,
			      transl_type cset env node_env ptyp2)

  | Ptyp_paren ptyp' ->
      transl_type cset env node_env ptyp'



let transl_constraint cset env node_env tyc =

  match tyc.ptyc_desc with

    Ptyc_ssk [] -> 
      ()

  | Ptyc_ssk (hd :: tl) ->
      let nd = transl_type cset env node_env hd in
      List.iter (function typ -> 
	Solver.same_skel nd (transl_type cset env node_env typ)
      ) tl

  | Ptyc_leq (lhs_list, rhs_list) ->
      Standard.iter_square (fun (ods1, ptyp1) (ods2, ptyp2) ->
	match ods1, ods2 with
	  NoDestr, NoDestr -> 
	    Solver.strong_leq
	      (transl_type cset env node_env ptyp1)
	      (transl_type cset env node_env ptyp2)
	| _ ->
	    Solver.weak_leq
	      begin match ods1 with
		NoDestr | Destr ->
		  Solver.Nd (transl_type cset env node_env ptyp1)
	      | SkDestr ->
		  Solver.Sk (Solver.nd_skeleton
			       (transl_type cset env node_env ptyp1))
	      end
	      begin match ods2 with
		NoDestr | Destr ->
		  Solver.Nd (transl_type cset env node_env ptyp2)
	      | SkDestr ->
		  Solver.Sk (Solver.nd_skeleton 
			       (transl_type cset env node_env ptyp2))
	      end
      ) lhs_list rhs_list



let transl_scheme env ptys =

  let cset = Solver.cset () in

  let kind_env = kind_scheme env ptys in
  let node_env = transl_variables cset kind_env in

  List.iter (transl_constraint cset env node_env) ptys.ptys_constraints;

  let atomic_variables =
    Standard.filter_map (function
	name, (Level | Row _) -> Some name
      |	_, Type -> None
    ) kind_env
  in      

  let skel_info = 
    match
      List.sort Pervasives.compare
	(List.map (fun (name, nd) -> Solver.skeleton_stamp nd, name) node_env)
    with
      [] -> []
    | (stamp, name) :: tl ->
	let rec collect accu (stamp0, name0) = function
	    [] -> accu
	  | (stamp, name) :: tl ->
	      if stamp = stamp0 
	      then collect ((name, name0) :: accu) (stamp0, name0) tl
	      else collect accu (stamp, name) tl
	in
	collect [] (stamp, name) tl
  in

  Stripinfo.store_scheme ptys.ptys_loc atomic_variables skel_info;

  cset, transl_type cset env node_env ptys.ptys_type
