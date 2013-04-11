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

(* $Id: parsetree.ml,v 1.9 2003/06/26 13:32:51 simonet Exp $ *)
(* Parsetree: The abstract syntax tree produced by parsing *)

open Asttypes



(* Level declaration *)

type level_declaration =
    { plvd_lb: level list;
      plvd_ub: level list;
      plvd_loc: Location.t
    } 

and level =
    { plvl_desc: level_desc;
      plvl_loc: Location.t
    } 

and level_desc =
    Plvl_ident of Longident.t
  | Plvl_principal of string

(* Type expressions for the core language *)

type core_type =
  { ptyp_desc: core_type_desc;
    ptyp_loc: Location.t }

and core_type_desc = 
    Ptyp_var of string
	       (* argument    pc          row         result      level *)
  | Ptyp_arrow of core_type * core_type * core_type * core_type * core_type
  | Ptyp_tuple of core_type list
  | Ptyp_constr of Longident.t * core_type list
  | Ptyp_row of Longident.t * core_type * core_type
                      (* arguments        1st pc      lst row     result *)

  (* The two following constructs can only appear in schemes *)
  | Ptyp_bounds of bool * level list * level list
	(* If the boolean is true, rows *)
  | Ptyp_arrow_abbrev of core_type list * core_type * core_type * core_type

  | Ptyp_paren of core_type

(* Type schemes for the core language *)

type destructor =
    NoDestr
  | Destr
  | SkDestr

type core_type_constraint_desc =
    Ptyc_ssk of core_type list
  | Ptyc_leq of (destructor * core_type) list * (destructor * core_type) list

type core_type_constraint =
    { ptyc_desc: core_type_constraint_desc;
      ptyc_loc: Location.t;
    }

type core_scheme = 
    { ptys_type: core_type;
      ptys_constraints: core_type_constraint list;
      ptys_loc: Location.t
    } 

(* Value expressions for the core language *)

type pattern =
  { ppat_desc: pattern_desc;
    ppat_loc: Location.t }

and pattern_desc =
    Ppat_any
  | Ppat_var of string
  | Ppat_alias of pattern * string
  | Ppat_constant of constant
  | Ppat_tuple of pattern list
  | Ppat_construct of Longident.t * pattern option * bool
  | Ppat_record of (Longident.t * pattern) list
  | Ppat_array of pattern list
  | Ppat_or of pattern * pattern
  | Ppat_constraint of pattern * core_scheme
  | Ppat_paren of pattern

type try_pattern =
    { ptry_desc: try_pattern_desc;
      ptry_loc: Location.t
    } 

and try_pattern_desc =
    Ptry_any
  | Ptry_list of try_pattern_item list

and try_pattern_item =
    { ptryi_exception: Longident.t;
      ptryi_arg: pattern option;
      ptryi_loc: Location.t }

type expression =
  { pexp_desc: expression expression_desc;
    pexp_loc: Location.t }

and 'a expression_desc =
    Pexp_ident of Longident.t
  | Pexp_constant of constant
  | Pexp_let of rec_flag * (pattern * expression) list * expression
  | Pexp_function of (pattern * (*when*) expression option * expression) list
  | Pexp_apply of 'a * 'a list
  | Pexp_match of 'a * (pattern * (*when*) expression option * expression) list
  | Pexp_raise of Longident.t * 'a option
  | Pexp_try of expression * (try_pattern * expression * try_case) list
  | Pexp_finally of expression * expression
  | Pexp_tuple of 'a list
  | Pexp_construct of Longident.t * 'a option * bool
  | Pexp_record of (Longident.t * 'a) list * 'a option
  | Pexp_field of 'a * Longident.t
  | Pexp_setfield of 'a * Longident.t * 'a
  | Pexp_array of 'a list
  | Pexp_ifthenelse of 'a * expression * expression option
  | Pexp_sequence of 'a * 'a
  | Pexp_while of expression * expression
  | Pexp_for of string * 'a * 'a * direction_flag * expression
  | Pexp_constraint of 'a * core_scheme
  | Pexp_assert of 'a
  | Pexp_assertfalse
  | Pexp_paren of 'a

(* Value descriptions *)

and value_description =
  { pval_type: core_scheme;
    pval_prim: string list}

(* Type declarations *)

and type_declaration =
  { ptype_params: (string * (type_argkind * type_argvariance)) list;
    ptype_fun: bool;
    ptype_cstrs: (core_type * core_type * Location.t) list;
    ptype_repr: type_repr;
    ptype_manifest: core_type option;
    ptype_loc: Location.t;

    (* location of identifier, required for OCaml code generation *)
    ptype_loc': Location.t; 

    (* Transient fields for topological sort of type declarations. *)
    mutable ptype_topo: int;
    mutable ptype_pred: string list;
  }

and type_argkind =
    Pak_var
  | Pak_level
  | Pak_type
  | Pak_row of Longident.t list

and type_argvariance =
    Pvar_none
  | Pvar_covariant
  | Pvar_contravariant
  | Pvar_invariant
  | Pvar_guarded

and type_repr =
    Ptype_abstract
  | Ptype_variant of 
      (string * core_type list) list * (core_type * Location.t) option
	(* The location is required for OCaml source code generation *)
  | Ptype_record of
      (string * mutable_flag * core_type) list * (core_type * Location.t) option
	(* The location is required for OCaml source code generation *)

(* Exception declarations *)

and exception_declaration = 
    { pexn_type: (string * Location.t) option * core_type list;
	(* The location is required for OCaml source code generation *)
      pexn_manifest: (Longident.t * Location.t) option;
	(* The location is required for OCaml source code generation *)
      pexn_loc: Location.t
    } 

(* Type expressions for the module language *)

and module_type =
  { pmty_desc: module_type_desc;
    pmty_loc: Location.t }

and module_type_desc =
    Pmty_ident of Longident.t
  | Pmty_signature of signature
  | Pmty_functor of 
      string * module_type * level list * level list * module_type
	* Location.t option
	(* The location is required for OCaml source code generation
	   (position of the arrow) *)
  | Pmty_with of module_type * with_constraint list
  | Pmty_paren of module_type

and signature = signature_item list

and signature_item =
  { psig_desc: signature_item_desc;
    psig_loc: Location.t }

and signature_item_desc =
    Psig_value of string * value_description
  | Psig_type of (string * type_declaration) list
  | Psig_level of string * level_declaration
  | Psig_exception of string * exception_declaration
  | Psig_module of string * module_type
  | Psig_modtype of string * modtype_declaration
  | Psig_open of Longident.t
  | Psig_include of module_type

and modtype_declaration =
    Pmodtype_abstract
  | Pmodtype_manifest of module_type

and with_constraint =
    { pwth_ident: Longident.t;
      pwth_desc: with_constraint_desc;
      pwth_loc: Location.t
    } 

and with_constraint_desc =
    Pwith_type of type_declaration
  | Pwith_level of level_declaration
  | Pwith_module of Longident.t

and flow =
    Pflo_leq of constant_level list * constant_level list
  | Pflo_eq of constant_level list

and constant_level =
    { pclvl_desc: Longident.t;
      pclvl_loc: Location.t }

(* Value expressions for the module language *)

and module_expr =
  { pmod_desc: module_expr_desc;
    pmod_loc: Location.t }

and module_expr_desc =
    Pmod_ident of Longident.t
  | Pmod_structure of structure
  | Pmod_functor of string * module_type * module_expr
  | Pmod_apply of module_expr * module_expr
  | Pmod_constraint of module_expr * module_type
  | Pmod_paren of module_expr

and structure = structure_item list

and structure_item =
  { pstr_desc: structure_item_desc;
    pstr_loc: Location.t }

and structure_item_desc =
    Pstr_eval of expression
  | Pstr_value of rec_flag * (pattern * expression) list
  | Pstr_primitive of string * value_description
  | Pstr_type of (string * type_declaration) list
  | Pstr_level of string * level_declaration
  | Pstr_exception of string * exception_declaration
  | Pstr_module of string * module_expr
  | Pstr_modtype of string * module_type
  | Pstr_open of Longident.t
  | Pstr_include of module_expr

(* Flows declaration *)

type flows_declaration = (string list * string list) list

(* Compilation units *)

type interface =
    { pint_signature: signature;
      pint_flows: flows_declaration;
      pint_pci: level list;
      pint_pcf: level list;
      pint_header_loc: Location.t
    } 

type implementation =
    { pimp_structure: structure;
      pimp_flows: flows_declaration;
      pimp_header_loc: Location.t
    } 

(* Toplevel phrases *)

type toplevel_phrase =
    Ptop_def of structure
  | Ptop_flow of flows_declaration
  | Ptop_dir of string * directive_argument

and directive_argument =
    Pdir_none
  | Pdir_string of string
  | Pdir_int of int
  | Pdir_ident of Longident.t
  | Pdir_bool of bool



