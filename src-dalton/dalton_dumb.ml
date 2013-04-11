(**************************************************************************)
(*                                                                        *)
(*                                  Dalton                                *)
(*                      an efficient implementation of                    *)
(*                 type inference with structural subtyping               *)
(*                                                                        *)
(*          Vincent Simonet, Projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*  Copyright 2002, 2003 Institut National de Recherche en Informatique   *)
(*  et en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with the   *)
(*  special exception on linking described in file LICENSE.               *)
(*                                                                        *)
(*  Author contact: Vincent.Simonet@inria.fr                              *)
(*  Software page: http://cristal.inria.fr/~simonet/soft/dalton/          *)
(*                                                                        *)
(**************************************************************************)

(* $Id: dalton_dumb.ml,v 1.2 2003/06/26 13:32:47 simonet Exp $ *)

open Dalton_aux
open Dalton_sig
open Format



module Make (Ground: GROUND) (Print: PRINT) (Draw: DRAW) (Report: ERROR_REPORT) 
= struct

  open Ground



  (*-----------------------------------------------------------------------*)
  (** {3 Constraint sets} *)

  type cset = unit
  let cset _ = ()
  let merge_cset _ _ = ()



  (*-----------------------------------------------------------------------*)
  (** {3 Terms} *)

  type node = unit
  let variable _ _ = ()
  let variable_in_sk _ = ()
  let typ _ _ = ()
  let row _ _ = ()
  let skeleton_stamp _ = 0



  (*-----------------------------------------------------------------------*)
  (** {3 Setting constraints} *)

  type skeleton = unit
  let set_expand_manifest _ = ()

  type node_or_skeleton =
      Nd of node
    | Sk of skeleton

  type unification_report
  exception UnificationError of unification_report
  let report_unification _ _ = ()

  let same_skel _ _ = ()
  let equal _ _ = ()
  let strong_leq _ _ = ()
  let weak_leq _ _ = ()
  let lower_bound _ _ = ()
  let upper_bound _ _ = ()



  (*-----------------------------------------------------------------------*)
  (** {3 Substitutions} *)

  type subst = {
      lb: Lb.t -> Lb.t;
      ub: Ub.t -> Ub.t;
      typ: 'a. 'a Type.t -> 'a Type.t;
      label: Label.t -> Label.t
    } 



  (*-----------------------------------------------------------------------*)
  (** {3 Schemes} *)

  module type SCHEME_ROOT = sig

    type t

    val cset: t -> cset
    val copy: cset -> (node -> node) -> t -> t

    val iter: (variance -> node -> unit) -> t -> unit
    val iter2: (variance -> node -> node -> unit) -> t -> t -> unit

    val fprint: formatter -> cset printer -> 
      (variance -> formatter -> node -> unit) -> t -> unit

  end

  module Scheme (Root: SCHEME_ROOT) = struct

    let copy ?subst r = r
    let non_generalizable _ = []
    let solve _ = ()
    type comparison_report
    let report_comparison _ _ = ()
    let compare _ _ = None
    let equivalent _ _ = true
    let fprint _ _ = ()
    let draw _ _ x y = (x, y)

  end

end
