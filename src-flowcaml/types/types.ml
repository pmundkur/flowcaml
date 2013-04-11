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

(* $Id: types.ml,v 1.6 2003/06/26 13:32:57 simonet Exp $ *)
(* Types *)

open Dalton_aux
open Asttypes
open Solver



(***************************************************************************)
(* Datatypes for representing types of modules. *)

type value_description = 
    { val_cset: cset;
      val_context: node Expr_context.t;
      val_typ: node
    } 

type constructor_description =
    { cstr_cset: cset;
      cstr_args: node list;
      cstr_res: node;
      cstr_level: node option;
      cstr_arity: int
    } 

type label_description = 
    { lbl_cset: cset;
      lbl_arg: node;
      lbl_res: node;
      lbl_level: node option;
      lbl_all: string array;
      lbl_pos: int;
      lbl_mut: mutable_flag
    } 

type type_declaration =
    { type_cset: cset;
      type_params: node list;
      type_kinds: Fkind.t list;
      type_prop: (variance * bool) list;
      type_fun: bool;
      type_repr: type_repr;
      type_manifest: node option
    } 

and type_repr =
    Type_abstract
  | Type_variant of (string * constructor_description) list
  | Type_record of (string * label_description) list

type level_declaration = 
    { lvd_lb: Level.Set.t;
      lvd_ub: Level.Set.t;
      lvd_lb_closed: Level.Set.t;
      lvd_ub_closed: Level.Set.t
    } 

type exception_declaration =
    { exn_cset: cset;
      exn_repr: exception_repr;
      exn_param: node option;
      exn_args: node list;
      exn_arity: int
    } 

and exception_repr =
    Exn_abstract
  | Exn_manifest of Path.t

type module_type =
    Tmty_ident of Path.t
  | Tmty_signature of signature
  | Tmty_functor of 
      Ident.t * module_type * Level.Set.t * Level.Set.t * module_type

and signature = signature_item list

and signature_item =
    Tsig_value of Ident.t * value_description
  | Tsig_type of Ident.t * type_declaration
  | Tsig_level of Ident.t * level_declaration
  | Tsig_exception of Ident.t * exception_declaration
  | Tsig_module of Ident.t * module_type
  | Tsig_modtype of Ident.t * modtype_declaration

and modtype_declaration =
    Tmodtype_abstract
  | Tmodtype_manifest of module_type

(* Compilation units *)

type interface =
    { int_signature: signature;
      int_lattice: Principal.lattice;
      int_pci: Level.Set.t;
      int_pcf: Level.Set.t
    } 
