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

(* $Id: transl_levelexpr.ml,v 1.4 2003/06/26 13:32:58 simonet Exp $ *)

open Parsetree
open Level
open Include_error



(***************************************************************************)
(** {2 Simple level expressions} *)

let transl_level env plvl =
  match plvl.plvl_desc with
    Plvl_ident lid ->
      let path, lvd = Env.lookup_level plvl.plvl_loc lid env in
      Tlvl_path path
  | Plvl_principal name ->
      Tlvl_principal name


let transl_level_set env list =
  let set =
    List.fold_left (fun set plvl ->
      Level.Set.add (transl_level env plvl) set
    ) Level.Set.empty list
  in
  set



(***************************************************************************)
(** {2 Initialize declarations} *)

let transl_initialize env ppci ppcf =
  let pci = transl_level_set env ppci in
  let pcf = transl_level_set env ppcf in
  pci, pcf


  let leq set1 set2 = 
    Level.Set.for_all (fun level1 ->
      Level.Set.exists (fun level2 -> Env.level_leq level1 level2) set2
    ) set1



let included_initialize env (pci1, pcf1) (pci2, pcf2) =

  if not (Solver.Ub.geq pci1 pci2) then
    raise (Error [Initialize (Rini_from (Solver.Ub.geq_report pci1 pci2, pci2))])
  else if not (Solver.Lb.leq pcf1 pcf2) then
    raise (Error [Initialize (Rini_to (Solver.Lb.leq_report pcf1 pcf2, pcf2))])

