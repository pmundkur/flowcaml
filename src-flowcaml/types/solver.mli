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

(* $Id: solver.mli,v 1.8 2003/06/26 13:32:57 simonet Exp $ *)
(* Solver: The Dalton instance. *)

open Format
open Dalton_aux



type 'a typ =
    { constr: Type_constructor.t;
      sons: 'a list
    } 



(***************************************************************************)

module Lb : sig

  type t = Level.Set.t

  val bottom: t
  val union: t -> t -> t
  val leq: t -> t -> bool
  val leq_report: t -> t -> Level.t

end



module Ub : sig

  type t = Level.Set.t

  val top: t
  val inter: t -> t -> t
  val geq: t -> t -> bool
  val geq_report: t -> t -> Level.t
      
end



module Lub : sig

  val leq: Lb.t -> Ub.t -> bool
  val fprint_notleq: formatter -> Lb.t -> Ub.t -> unit

end



(***************************************************************************)

module Report : sig

  type unification_message =
      ExprL | ExprR
    | PatternL | PatternR
    | Or_pattern
    | Or_pattern_var of string
    | Pattern_for of string
    | Merge_context of string
    | Abstract_context of string

  val unification_message : unification_message ref

end



(***************************************************************************)

(*-------------------------------------------------------------------------*)
(** {3 Constraint sets} *)

type cset
val cset: unit -> cset
val merge_cset: cset -> cset -> unit



(*-------------------------------------------------------------------------*)
(** {3 Terms} *)

type node
val variable: cset -> kind -> node
val variable_in_sk: node -> node
val typ: cset -> node typ -> node
val row: cset -> (Path.t * node * node) -> node



(*-------------------------------------------------------------------------*)
(** {3 Setting constraints} *)

type skeleton

val set_expand_manifest: (skeleton typ -> (node * node) option) -> unit

type node_or_skeleton =
    Nd of node
  | Sk of skeleton

type unification_report
exception UnificationError of unification_report
val report_unification: formatter -> unification_report -> unit

val same_skel: node -> node -> unit    
val equal: node -> node -> unit
val strong_leq: node -> node -> unit
val weak_leq: node_or_skeleton  -> node_or_skeleton -> unit
val lower_bound: Level.Set.t -> node_or_skeleton -> unit
val upper_bound: node_or_skeleton -> Level.Set.t -> unit

type subst = {
    lb: Level.Set.t -> Level.Set.t;
    ub: Level.Set.t -> Level.Set.t;
    typ: 'a. 'a typ -> 'a typ;
    label: Path.t -> Path.t
  } 


(*-------------------------------------------------------------------------*)
(** {3 Configuration of pretty-print} *)

module Printing : sig

  val all_variables: bool ref
  val ghost_variables: bool ref
  val polarities: bool ref

end



(*-------------------------------------------------------------------------*)
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




module Scheme (Root: SCHEME_ROOT) : sig

  exception Copy_expand

  val copy: ?subst:subst -> ?expand:(skeleton typ -> bool)
    -> Root.t -> Root.t

  type minimal_report
  val report_minimal: formatter -> minimal_report -> unit
  val has_minimal_instance: Root.t -> minimal_report option

  type solve_report
  val report_solve: formatter -> solve_report -> unit
  val solve: Root.t -> solve_report option
      
  type comparison_report
  val report_comparison: formatter -> comparison_report -> unit
  val compare: Root.t -> Root.t -> comparison_report option
  val equivalent: Root.t -> Root.t -> bool

  val fprint: formatter -> Root.t -> unit
  val draw: Draw.window -> Root.t -> int -> int -> int * int

end


(*-------------------------------------------------------------------------*)
(** {3 Undocumented functions} *)

val skeleton_stamp: node -> int
val get_lower_bound: node -> Level.Set.t
val get_upper_bound: node -> Level.Set.t
val nd_skeleton: node -> skeleton

