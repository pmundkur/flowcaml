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

(* $Id: include_error.ml,v 1.4 2003/06/30 18:05:34 simonet Exp $ *)
(* Include_error *)

open Format
open Types
open Print_types
open Dalton_aux



(***************************************************************************)

type initialize_report =
    Rini_from of Level.t * Level.Set.t
  | Rini_to of Level.t * Level.Set.t

let initialize_report ppf = function
    Rini_from (level, level_set) ->
      fprintf ppf
	"@[<hv>affects %a@ must be declared in the interface"
	  Level.fprint level

  | Rini_to (level, level_set) ->
      fprintf ppf
	"@[<hv>raises %a@ must be declared in the interface"
	  Level.fprint level



(***************************************************************************)

type type_report =
    Rtyp_arity_mismatch of int * int
  | Rtyp_kind_mismatch of int * Fkind.t * Fkind.t
  | Rtyp_variance_mismatch of int * variance * variance
  | Rtyp_annot_mismatch of int
  | Rtyp_fun_mismatch
  | Rtyp_repr_mismatch
  | Rtyp_constructor_mismatch of string
  | Rtyp_label_mismatch of string
  | Rtyp_manifest_mismatch


let type_report ppf = function
    Rtyp_arity_mismatch (provided, expected) ->
      fprintf ppf 
	"The expected declaration has %d parameter(s) \
	but the provided one has %d"
	expected provided
  | Rtyp_kind_mismatch (i, provided, expected) ->
      fprintf ppf
	"@[<hov>The type parameter '%s is expected to have kind '%a@ but \
	is provided with kind %a@]"
	(Misc.name_of_int i) 
	Fkind.fprint provided
	Fkind.fprint expected
  | Rtyp_variance_mismatch (i, provided, expected) ->
      fprintf ppf
	"@[<hov>The type parameter '%s is expected to be %a be is provided \
	as %a@]"
	(Misc.name_of_int i)
	Variance.fprint_name expected
	Variance.fprint_name provided
  | Rtyp_annot_mismatch i ->
      fprintf ppf
	"@[<hov>The type parameter '%s is expected to be marked #@]"
	(Misc.name_of_int i)
  | Rtyp_fun_mismatch ->
      fprintf ppf
	"@[This type constructor is expected to be marked noneq@]"
  | Rtyp_repr_mismatch ->
      fprintf ppf
	"@[The type representations do not match@]"
  | Rtyp_constructor_mismatch name ->
      fprintf ppf "@[Constructor %s mismatches.@]" name
  | Rtyp_label_mismatch name ->
      fprintf ppf "@[Field %s mismatches.@]" name
  | Rtyp_manifest_mismatch ->
      fprintf ppf "@[Manifest types mismatches.@]"



(***************************************************************************)

type level_report =
    Rlvd_missing of Level.t * Level.t

let level_report ppf = function
    Rlvd_missing (level1, level2) ->
      fprintf ppf "The inequality %a < %a is required but not provided"
	Level.fprint level1
	Level.fprint level2



(***************************************************************************)

type exception_report =
    Rexn_invalid_abstraction
  | Rexn_repr_mismatch
  | Rexn_arity_mismatch
  | Rexn_type_mismatch

let exception_report ppf = function
    Rexn_invalid_abstraction ->
      fprintf ppf "Manifest exception equality cannot be hidden."
  | Rexn_repr_mismatch ->
      fprintf ppf "Manifest representations do not match."
  | Rexn_arity_mismatch ->
      fprintf ppf "Arities do not match"
  | Rexn_type_mismatch ->
      fprintf ppf "Arguments types do not match"



(*************************************************************************)

type error =
    Missing_field of Ident.t
  | Value_descriptions of  Ident.t * value_description * value_description
	* Value_description.comparison_report
  | Type_declarations of Ident.t * type_declaration * type_declaration
	* type_report
  | Level_declarations of Ident.t * level_declaration * level_declaration
	* level_report
  | Exception_declarations of
      Ident.t * exception_declaration * exception_declaration * exception_report 
  | Module_types of module_type * module_type
  | Modtype_infos of Ident.t * modtype_declaration * modtype_declaration
  | Modtype_permutation
  | Initialize of initialize_report

exception Error of error list



(***************************************************************************)

let include_err ppf = function
  | Missing_field id ->
      fprintf ppf
	"The field `%a' is required but not provided" 
	Ident.fprint id

  | Value_descriptions(id, d1, d2, report) ->
      fprintf ppf
       "@[<v>@[<hv 2>Values mismatch: the provided value @ \
        %a@;<1 -2>is not included in the expected one@ %a@]@ %a@]"
       (fprint_value_description id) d1  (fprint_value_description id) d2
       Value_description.report_comparison report

  | Type_declarations(id, d1, d2, report) ->
      fprintf ppf
       "@[<v>@[<hv 2>Type declarations mismatch: the provided type declaration@ \
        %a@;<1 -2>is not included in the expected one@ %a@]@ %a@]"
	(fprint_type_declaration id) d1
	(fprint_type_declaration id) d2
	type_report report
      
  | Level_declarations(id, d1, d2, report) ->
      fprintf ppf
       "@[<v>@[\
	<hv 2>Level declarations mismatch: the provided type declaration@ \
        %a@;<1 -2>is not included in the expected one@ %a@]@ %a@]"
	(fprint_level_declaration id) d1
	(fprint_level_declaration id) d2
	level_report report
      
  | Exception_declarations(id, d1, d2, report) ->
      fprintf ppf
       "@[<v>@[<hv 2>Exception declarations do not match:@ \
        %a@;<1 -2>is not included in@ %a@]@ %a@]"
      (fprint_exception_declaration id) d1
      (fprint_exception_declaration id) d2
      exception_report report

  | Module_types(mty1, mty2)->
      fprintf ppf
       "@[<hv 2>Modules do not match:@ \
        %a@;<1 -2>is not included in@ %a@]"
	fprint_module_type mty1
	fprint_module_type mty2
  | Modtype_infos(id, d1, d2) ->
      fprintf ppf
       "@[<hv 2>Module type declarations do not match:@ \
        %a@;<1 -2>is not included in@ %a@]"
      (fprint_modtype_declaration id) d1
      (fprint_modtype_declaration id) d2
  | Modtype_permutation ->
      fprintf ppf "Illegal permutation of structure fields"

  | Initialize report ->
      initialize_report ppf report

let report_error ppf = function
  |  [] -> ()
  | err :: errs ->
      let print_errs ppf errs =
         List.iter (fun err -> fprintf ppf "@ %a" include_err err) errs 
      in
      fprintf ppf "@[<v>%a%a@]" include_err err print_errs errs
