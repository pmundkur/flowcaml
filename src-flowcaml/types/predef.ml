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

(* $Id: predef.ml,v 1.6 2003/06/26 13:32:57 simonet Exp $ *)
(* Predef: Predefined declarations *)

open Fkind
open Dalton_aux
open Longident
open Path
open Type_constructor
open Solver
open Types



(* *************************************************************************
   Functions and tuples
 *)

let funct t1 t2 t3 t4 t5 =
  { constr = { tc_desc = TCfunction;
	       tc_kinds = [Type; Level; Row []; Type; Level];
	       tc_prop = [Contravariant, false;
		       Contravariant, false;
		       Covariant, false;
		       Covariant, false;
		       Covariant, true];
	       tc_fun = true		 
	     };
    sons = [t1; t2; t3; t4; t5];
  } 



let tuple list =
  { constr = { tc_desc = TCtuple (List.length list);
	       tc_prop = List.map (fun _ -> Covariant, true) list;
	       tc_kinds = List.map (fun _ -> Type) list;
	       tc_fun = false
	     };
    sons = list
  } 



(* *************************************************************************
   Predefined type constructors
 *)

let transl_variable cset kind =
  Solver.variable cset (daltonize kind)


let decl constr type_kind =
  let cset = Solver.cset () in
  let sons = List.map (transl_variable cset) constr.tc_kinds in
  let decl =
    { type_cset = cset;
      type_params = sons;
      type_kinds = constr.tc_kinds;
      type_prop = constr.tc_prop;
      type_fun = constr.tc_fun;
      type_repr = type_kind;
      type_manifest = None
    } 
  in
  ignore (Type_declaration.solve decl);
  decl

let constr0 id =
  { tc_desc = TCpath (Pident id);
    tc_kinds = [];
    tc_prop = [];
    tc_fun = true
  }

let constr1 id =
  { tc_desc = TCpath (Pident id);
    tc_kinds = [Level];
    tc_prop = [Covariant, true];
    tc_fun = false
  }

let constr2 v id =
  { tc_desc = TCpath (Pident id);
    tc_kinds = [Type; Level];
    tc_prop = [v, false; Covariant, true];
    tc_fun = false
  } 



(* 'a int *)

let int_id = Ident.create "int"
let int_constr = constr1 int_id
let int_decl = decl int_constr Type_abstract
let int t = { constr = int_constr; sons = [t] } 

(* 'a int32 *)

let int32_id = Ident.create "int32"
let int32_constr = constr1 int32_id
let int32_decl = decl int32_constr Type_abstract
let int32 t = { constr = int32_constr; sons = [t] } 

(* 'a int64 *)

let int64_id = Ident.create "int64"
let int64_constr = constr1 int64_id
let int64_decl = decl int64_constr Type_abstract
let int64 t = { constr = int64_constr; sons = [t] } 

(* 'a nativeint *)

let nativeint_id = Ident.create "nativeint"
let nativeint_constr = constr1 nativeint_id
let nativeint_decl = decl nativeint_constr Type_abstract
let nativeint t = { constr = nativeint_constr; sons = [t] } 

(* 'a char *)

let char_id = Ident.create "char"
let char_constr = constr1 char_id
let char_decl = decl char_constr Type_abstract
let char t = { constr = char_constr; sons = [t] } 

(* ('a, 'b) charray *)

let charray_id = Ident.create "charray"
let charray_constr =
  { tc_desc = TCpath (Pident charray_id);
    tc_kinds = [Level; Level];
    tc_prop = [Invariant, false; Covariant, true];
    tc_fun = false
  } 
let charray_decl = decl charray_constr Type_abstract
let charray t1 t2 = { constr = charray_constr; sons = [t1; t2] } 

(* 'a string *)

let string_id = Ident.create "string"
let string_constr =
  { tc_desc = TCpath (Pident string_id);
    tc_kinds = [Level];
    tc_prop = [Covariant, true];
    tc_fun = false
  } 
let string_decl = decl string_constr Type_abstract
let string t1 = { constr = string_constr; sons = [t1] } 

(* 'a float *)

let float_id = Ident.create "float"
let float_constr = constr1 float_id
let float_decl = decl float_constr Type_abstract
let float t = { constr = float_constr; sons = [t] } 

(* 'a bool *)

let bool_id = Ident.create "bool"
let bool_constr = constr1 bool_id
let bool t = { constr = bool_constr; sons = [t] } 

let true_constructor =
  let cset = Solver.cset () in
  let lvl = Solver.variable cset Katom in
  { cstr_cset = cset;
    cstr_args = [];
    cstr_res = Solver.typ cset (bool lvl);
    cstr_level = Some lvl;
    cstr_arity = 0
  } 

let false_constructor =
  let cset = Solver.cset () in
  let lvl = Solver.variable cset Katom in
  { cstr_cset = cset;
    cstr_args = [];
    cstr_res = Solver.typ cset (bool lvl);
    cstr_level = Some lvl;
    cstr_arity = 0
  } 

let bool_decl = decl bool_constr
    (Type_variant ["false", false_constructor; "true", true_constructor])

(* unit *)

let unit_id = Ident.create "unit"
let unit_constr = constr0 unit_id
let unit = { constr = unit_constr; sons = [] } 

let void =
  let cset = Solver.cset () in
  let lvl = Solver.variable cset Katom in
  let lvl' = Solver.variable cset Katom in
  { cstr_cset = cset;
    cstr_args = [];
    cstr_res = Solver.typ cset unit;
    cstr_level = None;
    cstr_arity = 0
  } 

let unit_decl = decl unit_constr (Type_variant [ "()", void ])

(* ('a, 'b) array *)

let array_id = Ident.create "array"
let array_constr = constr2 Invariant array_id
let array_decl = decl array_constr Type_abstract
let array t1 t2 = { constr = array_constr; sons = [t1; t2] } 

(* ('a, 'b) list *)

let list_id = Ident.create "list"
let list_constr = constr2 Covariant list_id
let list t1 t2 = { constr = list_constr; sons = [t1; t2] }

let nil =
  let cset = Solver.cset () in
  let lvl = Solver.variable cset Katom in
  { cstr_cset = cset;
    cstr_args = [];
    cstr_res = Solver.typ cset (list (Solver.variable cset Ktype) lvl);
    cstr_level = Some lvl;
    cstr_arity = 0
  } 

let consse =
  let cset = Solver.cset () in
  let lvl = Solver.variable cset Katom in
  let arg = Solver.variable cset Ktype in
  { cstr_cset = cset;
    cstr_args =  [arg; Solver.typ cset (list arg lvl)];
    cstr_res = Solver.typ cset (list arg lvl);
    cstr_level = Some lvl;
    cstr_arity = 2
  } 

let list_decl = decl list_constr (Type_variant ["[]", nil; "::", consse])

(* ('a, 'b) option *)

let option_id = Ident.create "option"
let option_constr = constr2 Covariant option_id
let option t1 t2 = { constr = option_constr; sons = [t1; t2] } 

let none =
  let cset = Solver.cset () in
  let lvl = Solver.variable cset Katom in
  { cstr_cset = cset;
    cstr_args = [];
    cstr_res = Solver.typ cset (option (Solver.variable cset Ktype) lvl);
    cstr_level = Some lvl;
    cstr_arity = 0
  } 

let some =
  let cset = Solver.cset () in
  let lvl = Solver.variable cset Katom in
  let arg = Solver.variable cset Ktype in
  { cstr_cset = cset;
    cstr_args = [arg];
    cstr_res = Solver.typ cset (option arg lvl);
    cstr_level = Some lvl;
    cstr_arity = 1
  } 

let option_decl = decl option_constr (Type_variant ["None", none; "Some", some])



let types =
  [ int_id, int_decl;
    int32_id, int64_decl;
    int64_id, int64_decl;
    nativeint_id, nativeint_decl;
    char_id, char_decl;
    charray_id, charray_decl;
    string_id, string_decl;
    float_id, float_decl;
    bool_id, bool_decl;
    unit_id, unit_decl;
    array_id, array_decl;
    list_id, list_decl;
    option_id, option_decl
  ] 



(* *************************************************************************
   Predefined exceptions
 *)

let mkexn mkargs =
  let cset = Solver.cset () in
  let lvl = Solver.variable cset Katom in
  let args = List.map (Solver.typ cset) (mkargs lvl) in
  let arity = List.length args in

  { exn_cset = cset;
    exn_repr = Exn_abstract;
    exn_param = Some lvl;
    exn_args = args;
    exn_arity = arity
  } 

let exceptions =
  [
(*    "Match_failure", mkexn (fun lvl -> [string lvl lvl; int lvl; int lvl]); *)
(*    "Out_of_memory", mkexn (fun lvl -> []); *)
(*    "Stack_overflow", mkexn (fun lvl -> []); *)
    "exit", mkexn (fun lvl -> []);
    "Invalid_argument", mkexn (fun lvl -> [string lvl]);
    "Failure", mkexn (fun lvl -> [string lvl]);
    "Not_found", mkexn (fun lvl -> []);
(*    "Sys_blocked_io", mkexn (fun lvl -> []); *)
(*    "Sys_error", mkexn (fun lvl -> [string lvl lvl]); *)
    "End_of_file", mkexn (fun lvl -> []);
    "Division_by_zero", mkexn (fun lvl -> []);
(*    "Assert_failure", mkexn (fun lvl -> [string lvl lvl; int lvl; int lvl])  *)
  ] 



(* *************************************************************************
   Predefined environment
 *)


let env =

  let env_types = 
    List.fold_left (fun env (id, decl) ->
      Env.store_type id (Pident id) decl env
    ) Env.empty
    types
  in

  List.fold_left (fun env (s, decl) ->
    snd (Env.enter_exception s decl env)
  ) env_types
    exceptions

let initial () =
  try
    if !Clflags.nopervasives
    then env
    else Env.open_pers_signature "Pervasives" env
  with Not_found ->
    Misc.fatal_error "cannot open pervasives.fcmi"
