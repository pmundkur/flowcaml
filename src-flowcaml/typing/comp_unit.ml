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

(* $Id: comp_unit.ml,v 1.5 2003/06/26 13:32:58 simonet Exp $ *)
(* Type_uc *)

open Format
open Parsetree
open Types
open Print_types
open Typedtree
open Type_mod



(* Translate an interface *)

let transl_interface env pint =

  let lat = Principal.translate pint.pint_flows in
  let pci, pcf = 
    Transl_levelexpr.transl_initialize env pint.pint_pci pint.pint_pcf 
  in

  Env.set_current_lat lat;

  let sg = transl_signature (Predef.initial ()) pint.pint_signature in

  { int_signature = sg;
    int_lattice = lat;
    int_pci = pci;
    int_pcf = pcf
  } 



(* Type an implementation *)

let type_implementation env pimp =

  let lat = Principal.translate pimp.pimp_flows in

  Env.set_current_lat lat;

  let str, sg, pci, pcf, final_env = 
    type_structure env Solver.Lb.bottom pimp.pimp_structure 
  in

  { imp_structure = str;
    imp_interface = { int_signature = sg;
		      int_lattice = lat;
		      int_pci = pci;
		      int_pcf = pcf
		    } 
  } 



(***************************************************************************)

type comparison_error_desc =
    Flow_mismatch of string * string
  | Initialize_mismatch of Include_error.initialize_report
  | Signature_mismatch of Include_error.error list

exception Comparison_error of string * string * comparison_error_desc

let comparison_error impl_name intf_name desc =
  raise (Comparison_error (impl_name, intf_name, desc))

let report_error ppf impl_name intf_name desc =

  let report_desc ppf = function

      Flow_mismatch (name1, name2) -> 
	fprintf ppf
	  "@[<hv 2>It requires the flow @[!%s < !%s@],@;<1 -2>\
	  which is not declared in the interface.@]"
	  name1 name2

    | Initialize_mismatch report ->
	Include_error.initialize_report ppf report

    | Signature_mismatch reasons ->
	fprintf ppf
	  "@[<v>Signatures do not match.@ %a@]"
	  Include_error.report_error reasons

  in

  fprintf ppf
    "@[<v>@[The implementation %s@ does not match the interface %s:@]@ %a@]"
    impl_name
    intf_name
    report_desc desc



(* Check that an implementation of a compilation unit meets its
   interface. *)

let included impl_name impl_int intf_name decl_int =

  (* Lattices comparison. *)
  begin 
    match Principal.included impl_int.int_lattice decl_int.int_lattice with
      None -> ()
    | Some (name1, name2) ->
	comparison_error impl_name intf_name (Flow_mismatch (name1, name2))
  end;
  
  let env = Predef.initial () in
  Env.set_current_lat decl_int.int_lattice;
  Env.set_current_env env;

  (* Initialize comparison *)
  begin
    try
      Transl_levelexpr.included_initialize env
	(impl_int.int_pci, impl_int.int_pcf)
	(decl_int.int_pci, decl_int.int_pcf)
    with
      Include_error.Error [Include_error.Initialize report] -> 
	comparison_error impl_name intf_name (Initialize_mismatch report)
  end;

  (* Signatures comparison. *)
  begin
    try
      Include_mod.signatures env 
	impl_int.int_signature decl_int.int_signature
    with Include_error.Error reasons ->
      comparison_error impl_name intf_name (Signature_mismatch reasons)
  end
