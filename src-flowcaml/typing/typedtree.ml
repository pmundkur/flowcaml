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

(* $Id: typedtree.ml,v 1.3 2003/06/26 13:32:59 simonet Exp $ *)
(* Abstract syntax tree after typing *)

open Misc
open Asttypes
open Types
open Solver



(* Value expressions for the core language *)

type pattern =
  { pat_desc: pattern_desc;
    pat_loc: Location.t;
    pat_cset: cset;
    pat_context: node Pat_context.t;
    pat_typ: node;
    pat_level: node;
    pat_env: Env.t
  }

and pattern_desc =
    Tpat_any
  | Tpat_var of Ident.t
  | Tpat_alias of pattern * Ident.t
  | Tpat_constant of constant
  | Tpat_tuple of pattern list
  | Tpat_construct of constructor_description * pattern option
  | Tpat_record of (label_description * pattern) list
  | Tpat_array of pattern list
  | Tpat_or of pattern * pattern * Path.t option



type try_pattern_desc =
    Ttry_any
  | Ttry_list of (exception_declaration * pattern option) list

and try_pattern =
    { try_desc: try_pattern_desc;
      try_loc: Location.t;
      try_cset: cset;
      try_context: node Pat_context.t;
      try_handled: node;
      try_reraise: node;
      try_remainder: node;
      try_level: node;
      try_env: Env.t
    }



type partial = Partial | Total

type expression =
  { exp_desc: expression_desc;
    exp_loc: Location.t;
    exp_cset: cset;
    exp_context: node Expr_context.t;
    exp_pc: node;
    exp_typ: node;
    exp_row: node;
    exp_env: Env.t
  }

and expression_desc =
    Texp_ident of Path.t * value_description option
  | Texp_constant of constant
  | Texp_let of rec_flag * (pattern * expression) list * expression
  | Texp_function of (pattern * expression option * expression) list * partial
  | Texp_apply of expression * expression list
  | Texp_match of expression * (pattern * expression option * expression) list
	* partial
  | Texp_raise of exception_declaration * expression option
  | Texp_try of expression * (try_pattern * expression * try_case) list
  | Texp_finally of expression * expression
  | Texp_tuple of expression list
  | Texp_construct of constructor_description * expression option
  | Texp_record of (label_description * expression) list * expression option
  | Texp_field of expression * label_description
  | Texp_setfield of expression * label_description * expression
  | Texp_array of expression list
  | Texp_ifthenelse of expression * expression * expression option
  | Texp_sequence of expression * expression
  | Texp_while of expression * expression
  | Texp_for of
      Ident.t * expression * expression * direction_flag * expression
  | Texp_when of expression * expression
  | Texp_assert of expression
  | Texp_assertfalse

(* Value expressions for the module language *)

and module_expr =
  { mod_desc: module_expr_desc;
    mod_loc: Location.t;
    mod_type: module_type;
    mod_env: Env.t
  }

and module_expr_desc =
    Tmod_ident of Path.t
  | Tmod_structure of structure
  | Tmod_functor of Ident.t * module_type * module_expr
  | Tmod_apply of module_expr * module_expr * module_coercion
  | Tmod_constraint of module_expr * module_type * module_coercion

and structure = structure_item list

and structure_item =
    Tstr_eval of expression
  | Tstr_value of rec_flag * (pattern * expression) list
  | Tstr_primitive of Ident.t * value_description
  | Tstr_type of (Ident.t * type_declaration) list
  | Tstr_level of Ident.t * level_declaration
  | Tstr_exception of Ident.t * exception_declaration
  | Tstr_exn_rebind of Ident.t * Path.t
  | Tstr_module of Ident.t * module_expr
  | Tstr_modtype of Ident.t * module_type
  | Tstr_open of Path.t
  | Tstr_include of module_expr * Ident.t list

and module_coercion =
    Tcoerce_none
  | Tcoerce_structure of (int * module_coercion) list
  | Tcoerce_functor of module_coercion * module_coercion
(*  | Tcoerce_primitive of Primitive.description TEMPORARY *)


(* Compilation units *)

type implementation =
    { imp_structure: structure;
      imp_interface: interface
    } 
