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

(* $Id: transl_value.ml,v 1.3 2003/06/26 13:32:58 simonet Exp $ *)
(* Transl_value: translating value declarations *)

open Parsetree
open Fkind
open Types
open Transl_typeexpr
open Format



(***************************************************************************)

let translate env pval =

  let cset, typ = transl_scheme env pval.pval_type in

  let vald = 
    { val_cset = cset;
      val_context = Expr_context.empty;
      val_typ = typ
    }
  in

  match Value_description.solve vald with
    None -> vald
  | Some report -> 
      Type_core.error (pval.pval_type.ptys_loc, Type_core.SolveValue report)



let enter env name pval =

  let vald = translate env pval in
  let (id, newenv) = Env.enter_value name (Some vald) env in
  (id, vald, newenv)
 


(***************************************************************************)

open Include_error

let included env id decl1 decl2 =

  assert (decl1.val_context = Expr_context.empty);
  assert (decl2.val_context = Expr_context.empty);

  match Value_description.compare decl1 decl2 with
    None -> ()
  | Some report -> 
      raise(Error[Value_descriptions(id, decl1, decl2, report)])

