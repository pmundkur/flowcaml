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

(* $Id: dalton.mli,v 1.6 2003/06/26 13:32:46 simonet Exp $ *)


(** The core of the library
 *)

open Dalton_aux
open Dalton_sig
open Format



(** The constraint solver comes as a functor parametrized by four modules
    whose respective expected signatures are given in {!Dalton_sig}.
 *)
module Make (Ground: GROUND) (Print: PRINT) (Draw: DRAW) (Report: ERROR_REPORT) 
: sig

  open Ground



  (*-----------------------------------------------------------------------*)
  (** {3 Constraint sets} *)

  (** Constraint sets are represented by values of type [cset]. 
   *)
  type cset

  (** Each invokation of [cset ()] returns a new fresh empty constraint
      set.
   *)
  val cset: unit -> cset

  (** [merge_cset cs1 cs2] merges the two constraint sets [cs1] and [cs2].
      After invoking this function, [cs1] and [cs2] point to the same
      constraint set [cs] which corresponds to the conjunction of the
      previous [cs1] and [cs2].
   *)
  val merge_cset: cset -> cset -> unit



  (*-----------------------------------------------------------------------*)
  (** {3 Terms} *)

  (** (Multi-equations of) Terms are represented by values of type [node].
   *)
  type node

  (** [variable cs k] returns a fresh variable term of kind [k].  This
      variable may be used in the constraint set [cs].
   *)
  val variable: cset -> kind -> node

  (** [variable_in_sk nd] returns a fresh variable belonging to the same
      skeleton (and the same constraint set) as the node [nd].
   *)
  val variable_in_sk: node -> node

  (** [typ cs t] returns a fresh type term described by the type constructor
      [t] in the constraint set [cs].  Every son of [t] must be a node
      belonging to [cs].
   *)
  val typ: cset -> node Type.t -> node

  (** [row cs (lbl, nd_lbl, nd')] returns a fresh row node representing
      the term [lbl: nd_lbl, nd'] in the constraint set [cs].  [nd_lbl] and
      [nd'] must both belong to [cs].
   *)      
  val row: cset -> (Label.t * node * node) -> node



  (*-----------------------------------------------------------------------*)
  (** {3 Setting constraints} *)

  (** Multi-skeletons are represented by values of type [skeleton].
   *)
  type skeleton

  (** A value of type [node_or_skeleton] is either a node or a
      skeleton.  Such values are used to represent weak inequalities.
   *)
  type node_or_skeleton =
      Nd of node
    | Sk of skeleton

  (** Unification errors are described by a value of type 
      [unification_report].  The implementation of this type is not
      described.  As a result, reports may be used only in order to
      print error messages thanks to the function [report_unification].
   *)
  type unification_report

  (** Above functions report unification errors by raising an 
      exception [UnificationError] with an appropriate report as
      argument.
   *)
  exception UnificationError of unification_report

  (** [report_unification ppf r] pretty prints an error message on the
      formatter [ppf] describing the unification error reported by [r].
   *)
  val report_unification: formatter -> unification_report -> unit

  (** [same_skel nd1 nd2] sets a constraint [nd1 ~ nd2].  [nd1] and [nd2]
      must be nodes of the same constraint set and the same kind.  If
      setting this constrain entails an unification error, an exception
      [UnificationError] is raised.
   *)
  val same_skel: node -> node -> unit    

  (** [equal nd1 nd2] sets a constraint [nd1 = nd2].  [nd1] and [nd2]
      must be nodes of the same constraint set and the same kind. If
      setting this constrain entails an unification error, an exception
      [UnificationError] is raised.
   *)
  val equal: node -> node -> unit

  (** [strong_leq nd1 nd2] sets the strong inequality [ns1 < ns2].  [nd1]
      and [nd2] must be nodes of the same constraint set and the same kind.
      If setting this constrain entails an unification error, an exception
      [UnificationError] is raised.
   *)
  val strong_leq: node -> node -> unit

  (** [weak_leq ns1 ns2] sets a weak inequality [ns1 < ns2].  [ns1] and
      [ns2] must be nodes or skeletons of the same constraint set.
   *)
  val weak_leq: node_or_skeleton  -> node_or_skeleton -> unit

  (** [lower_bound lb ns] sets the weak inequality [lb < ns].
   *)
  val lower_bound: Lb.t -> node_or_skeleton -> unit

  (** [upper_bound ns ub] sets the weak inequality [ns < ub].
   *)
  val upper_bound: node_or_skeleton -> Ub.t -> unit



  (*-----------------------------------------------------------------------*)
  (** {3 Substitutions} *)

  (** A substitution may be applied while copying a scheme.  It is defined
      by a record of four functions of type [subst].
   *)
  type subst = {
      lb: Lb.t -> Lb.t;
	(** The substitution applied on lower bounds appearing in
	    the constraint set. *)
      ub: Ub.t -> Ub.t;
	(** The substitution applied on upper bounds appearing in
	    the constraint set. *)
      typ: 'a. 'a Type.t -> 'a Type.t;
	(** The substitution applied on type constructors. *)
      label: Label.t -> Label.t
	(** The substitution applied on row labels. *)
    } 



  (*-----------------------------------------------------------------------*)
  (** {3 Configuration of pretty-printing} *)

  (** Pretty-printing may be configured at run time by the flags of the
      module [Printing].
   *)
  module Printing : sig

    (** If [true], all variables are printed.  Default value is [false].
     *)
    val all_variables: bool ref

    (** If [true], unconstrained variables are printed as an underscore 
        rather than with a name.  Default value is [true].
     *)
    val ghost_variables: bool ref

    (** If [true], the polarity of each occurence of a variable is printed
        after its name.  Default value is [false].
     *)
    val polarities: bool ref
  end



  (*-----------------------------------------------------------------------*)
  (** {3 Schemes} *)

  (** A (type) scheme is made of a constraint set and a series of entry
      nodes, its roots.  The same instance of the library may deal with
      several form of schemes.  Each of them has to be described by an
      implementation of the signature [SCHEME_ROOT].
   *)
  module type SCHEME_ROOT = sig

    (** The type of schemes. *)
    type t

    (** [cset sh] returns the constraint set of the scheme [sh]. *)
    val cset: t -> cset

    (** [copy cset' f sh] creates a new scheme [sh'] as follows:
        - the constraint set of [sh'] is [cset'],
        - each root of [sh'] is obtained by applying [f] on the 
          corresponding root of [sh].
     *)
    val copy: cset -> (node -> node) -> t -> t

    (** [iter f sh] applies [f] on every root of [sh] (with the 
        variance of the root as first argument). 
     *)
    val iter: (variance -> node -> unit) -> t -> unit

    (** [iter2 f sh1 sh2] applieas [f] on every pair of corresponding 
        roots of [sh1] and [sh2] (with the variance of the roots as first
        argument).
     *)
    val iter2: (variance -> node -> node -> unit) -> t -> t -> unit

    (** [fprint ppf print_cset print_node sh] pretty prints the scheme
        [sh] on the formatter [ppf].  Two functions are provided as argument
        to allow printing of information handled by the solver:
        - [print_cset ppf cset] prints the constraint set [cset] on
          the formatter [ppf]
        - [print_node v ppf nd] prints the node [nd] of variance [v] on
          the formatter [ppf].
     *)
    val fprint: formatter -> cset printer -> 
      (variance -> formatter -> node -> unit) -> t -> unit

  end

  (** The functor scheme allows to build an implementation of functions
      dealing which each considered form schemes.
   *)
  module Scheme (Root: SCHEME_ROOT) : sig

    exception Copy_expand

    (** [copy ?subst sh] returns a fresh copy of the type scheme sh.
        No particular assumption is made about the type scheme [sh], but, 
        for efficiency, it is more than a good idea to solve it previously.
     *)
    val copy: ?subst:subst -> ?expand:(skeleton Type.t -> bool)
      -> Root.t -> Root.t

    (** [fprint ppf sh] pretty-prints the scheme [sh] on the formatter [ppf].
        The scheme [sh] is assumed to be solved.
     *)
    val fprint: formatter -> Root.t -> unit

    (** [draw window sh x y] draws the scheme [sh] on the window [window].
        The bottom left corner of the drawing has coordinates [x] and [y]
        and the function returns the coordinates of the upper right 
        corner.
     *)
    val draw: Draw.window -> Root.t -> int -> int -> int * int



    (** {3 Resolution} *)

    (** A solve report records an explanation of why the resolution
        of a scheme fails.
     *)
    type solve_report

    (** [report_solve ppf r] pretty prints an error message on the
        formatter [ppf] corresponding to the comparison report [r].
     *)
    val report_solve: formatter -> solve_report -> unit

    (** [solve sh] solves the scheme [sh].  If this function
        returns [None] then the scheme [sh] has some instances.  
        Moreover, it is stored in a "solved" form which is preserved
        as long as no term or constraint is added to its constraint
        set.  
     *) 
    val solve: Root.t -> solve_report option



    (** {3 Comparison} *)

    (** A comparison report records an explanation of the failure of
        the comparison of two schemes.
     *)
    type comparison_report

    (** [report_comparison ppf r] pretty prints an error message on the
        formatter [ppf] describing the comparison report [r].
     *)
    val report_comparison: formatter -> comparison_report -> unit

    (** [compare sh1 sh2] test wether [sh2] is more general than [sh1] 
        (i.e. [sh2] is a correct implementation of [sh1]).  It returns
        [None] if [sh2] is effectively so.  Otherwise, it returns [Some r]
        when [r] is a report "explaining" why [sh2] is not more general
        than [sh1].  The current implementation assumes that [sh1] and [sh2]
        are solved.
     *)
    val compare: Root.t -> Root.t -> comparison_report option

    (** [equivalent sh1 sh2] returns a boolean indicating wether the
        type schemes sh1 and sh2 are equivalent.  The current implementation
        assumes that [sh1] and [sh2] are in solved form.
     *)
    val equivalent: Root.t -> Root.t -> bool

    (** {3 Minimal instances} *)

    (** A comparison report records an explanation of why a scheme
        has no minimal instance.
     *)
    type minimal_report

    (** [report_minimal ppf r] pretty prints an error message on the
        formatter [ppf] describing the report [r].
     *)
    val report_minimal: formatter -> minimal_report -> unit

    (** [has_minimal_instance sh] tests wether the scheme [sh] has a minimal
        instance.  If so, the function returns [None].  Otherwise, it 
        returns [Some r] where [r] is a value of type [minimal_report]
        "explaining" why [sh] has no minimal instance.  The current
        implementation assumes that [sh] is in solved form.
     *)
    val has_minimal_instance: Root.t -> minimal_report option

  end



  (**/**)
  (*-----------------------------------------------------------------------*)
  (** {3 Undocumented functions} *)

  val set_expand_manifest: (skeleton Type.t -> (node * node) option) -> unit
  val skeleton_stamp: node -> int
  val get_lower_bound: node -> Lb.t
  val get_upper_bound: node -> Ub.t
  val nd_skeleton: node -> skeleton  

end
