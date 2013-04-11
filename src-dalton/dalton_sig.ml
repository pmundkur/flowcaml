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

(* $Id: dalton_sig.ml,v 1.5 2003/06/26 13:32:47 simonet Exp $ *)

(** Library module parameters.

    The Dalton solver is parametrized by several modules, which allow
    defining the term algebra, pretty-print, drawing and errors
    report.  This module gives the expected signatures of this
    modules.
  *)

open Format
open Dalton_aux



(***************************************************************************)
(** {2 The ground algebra} *)

(** The ground term algebra is specified by an implementation of the 
    signature [GROUND].  It must defines datatypes for constant bounds,
    type constructors and row labels; and simultaneously operations on
    them.  One may distinguish two categories of such operations:
    1. Algebraic operations, which allows manipulating the mathematical
       properties of the provided objects,
    2. Computational operations, which relate merely the internal
       representation of these objects and allow efficient algorithms
       (e.g. hashing, comparison, pretty-print...)
 *)
module type GROUND = sig

  (*-----------------------------------------------------------------------*)
  (** {3 Constant bounds} 

      The client must provide two sets of atomic constants, one for 
      representing variables lower bounds and another one for upper
      bounds.  These two sets must be equipped with a semi-lattice 
      structure.
   *)

  (** The module [Lb] specifies the set of constant lower bounds.
   *)
  module Lb : sig

    (** The type of constant lower bound.
     *)
    type t
	
    (** [bottom] is a distinguished lower bound.  It is the bottom 
        element of the semi-lattice.
     *)
    val bottom: t

    (** [is_bottom lb] must return [true] if and only if [lb] is [bottom].
     *)
    val is_bottom: t -> bool

    (** [union lb1 lb2] gives the union (according to the semi-lattice
        structure) of the lower bounds [lb1] and [lb2].
     *)
    val union: t -> t -> t

    (** [leq env lb1 lb2] tells wether [lb1] is less than or equal to
        [lb2] in the semi-lattice of constant lower bounds, i.e.:
        for all alpha, [lb2 < alpha] implies [lb1 < alpha].
     *)
    val leq: t -> t -> bool

    (** A comparison function on constant lower bounds has to be 
        provided.  It is just used for computation and has no semantical
        meaning.
     *)
    val compare: t -> t -> int

    (** [normalize lb] internally normalizes the lower bound lb.
     *)
    val normalize: t -> t

    (** [fprint ppf lb] pretty-prints the constant lower bound [lb] on
        the formatter [ppf].  (This function is used for printing of 
        constants in constraints.)
     *)
    val fprint: formatter -> t -> unit

    (** [fprint_in_term ppf lb] is used to prints the constant lower
        bound [lb] on the formatter [ppf] when it appears in a term, in
        place of a non-negative variable which has no predecessor.

        An usual implementation may be:
        let fprint_in_term _ ppf lb =
          Format.fprintf ppf "[> %a]" fprint lb
     *)
    val fprint_in_term: int -> formatter -> t -> unit

    val draw: t -> string list

  end



  (** The module [Ub] specifies the set of constant upper bounds.
   *)
  module Ub : sig

    (** The type of constant upper bound.
     *)
    type t
	
    (** [top] is a distinguished upper bound.  It is the top
        element of the semi-lattice.
     *)
    val top: t

    (** [is_top lb] must return [true] if and only if [lb] is [top].
     *)
    val is_top: t -> bool

    (** [inter lb1 lb2] gives the intersection (according to the
        semi-lattice structure) of the lower bounds [lb1] and [lb2].
     *)
    val inter: t -> t -> t

    (** [geq lb1 lb2] tells wether [lb1] is greater than or equal to
        [lb2] in the semi-lattice of constant upper bounds.
     *)
    val geq: t -> t -> bool

    (** A comparison function on constant upper bounds has to be 
        provided.  It is just used for computation and has no semantical
        meaning.
     *)
    val compare: t -> t -> int

    (** [normalize lb] internally normalizes the lower bound lb.
     *)
    val normalize: t -> t

    (** [fprint ppf ub] pretty-prints the constant upper bound [ub] on
        the formatter [ppf].  (This function is used for printing of 
        constants in constraints.)
     *)
    val fprint: formatter -> t -> unit

    (** [fprint_in_term ppf ub] is used to print the constant lower
        bound [ub] on the formatter [ppf] when it appears in a term, in
        place of a non-negative variable which has no predecessor.

        An usual implementation may be:
        let fprint_in_term _ ppf ub =
          Format.fprintf ppf "[< %a]" fprint ub
     *)
    val fprint_in_term: int -> formatter -> t -> unit

    val draw: t -> string list

  end


  (** The module [Lub] provides functions relating lower and upper bounds.
   *)
  module Lub : sig

    (** [leq lb ub] returns [true] if and only if [lb] is less than
        or equal to [ub], i.e. there exists some [alpha] such that 
        [lb < alpha] and [alpha < ub].
     *)
    val leq: Lb.t -> Ub.t -> bool

    (** [geq lb ub] returns [true] if and only if [lb] is greater than
        or equal to [ub], i.e. for all [alpha] and [beta], [alpha < ub]
        and [lb < beta] implies [alpha < beta].
     *)
    val geq: Lb.t -> Ub.t -> bool

    (** [fprint_in_term ppf lb ub] is used to print a pair of a
        lower bound and a upper bound in a term.

        An usual implementation may be
        let fprint_in_term _ ppf lb ub =
          Format.fprintf ppf "[> %a |< %a]" Lb.fprint lb  Ub.fprint ub
     *)
    val fprint_in_term: int -> formatter -> Lb.t -> Ub.t -> unit

    val fprint_notleq: formatter -> Lb.t -> Ub.t -> unit

  end



  (*-----------------------------------------------------------------------*)
  (** {3 Row labels} *)

  (** The set of row labels is defined by the module [Label].
   *)
  module Label : sig

    (** The type of row labels. 
     *)
    type t

    (** A function [compare] definining a total order on row labels must
        be provided.  This order is used for governing label mutations.
     *)
    val compare: t -> t -> int

    (** [hash lbl] returns a hash integer of the label [lbl].  If
        [compare lbl1 lbl2] returns [0] then [hash lbl1] and [hash lbl2]
        must return the same integer.
     *)
    val hash: t -> int

    (** [fprint ppf lbl] pretty-prints the row label [lbl] on the 
        formatter [ppf].
     *)
    val fprint: formatter -> t -> unit

  end



  (*-----------------------------------------------------------------------*)
  (** {3 Type constructors} *)


  (** Type constructors are given by the module [Type]. 
   *)
  module Type : sig

    (** A type constructor (with its arguments) is represented by a
        value of type ['a t], where ['a] is the type of the arguments.
     *) 
    type 'a t

    (** The boolean constant [ldestr_inv] tells wether there exists a
        type constructor for which the left destructor propagates on
        an invariant argument.
     *)
    val ldestr_inv: bool

    (** The boolean constant [ldestr_inv] tells wether there exists a
        type constructor for which the right destructor propagates on
        an invariant argument.
     *)
    val rdestr_inv: bool

    (** [ldestr t] returns [false] if the type [t] cannot be an argument
        of the left destructor.
     *)
    val ldestr: 'a t -> bool

    (** [rdestr t] returns [false] if the type [t] cannot be an argument
        of the right destructor.
     *)
    val rdestr: 'a t -> bool

    (** [compatible t1 t2] indicates wether the type constructors
        [t1] and [t2] are compatible.
     *)
    val compatible: 'a t -> 'a t -> bool

    (** [map f t] returns the constructor [t'] obtained by replacing
        every son [x] of [t] by [f i x] (where [i] is the information
        of variance, kind and destructor propagation associated to the
        argument [x] in [t]).
     *)
    val map: (constructor_arg -> 'a -> 'b) -> 'a t -> 'b t

    (** [iter f t] applies [f] on every son [x] of [t].
     *)
    val iter: (constructor_arg -> 'a -> unit) -> 'a t -> unit

    (** Given two compatible constructors [t1] and [t2], [map2 f t1 t2]...
        The result is not specified if [t1] and [t2] do not correspond. 
     *)
    val map2: (constructor_arg -> 'a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t

    (** Given two compatible constructors [t1] and [t2], [iter2 f t1 t2]
        applies [f] on every pair of corresponding sons of [t1] and [t2].
        The result is not specified if [t1] and [t2] do not correspond. 
     *)
    val iter2: (constructor_arg -> 'a -> 'b -> unit) -> 'a t -> 'b t -> unit

    (** Given two compatible constructors [t1] and [t2], [for_all f t1 t2]
        tests wether for all pair of sons [x1] and [x2], [f i x1 x2] is 
        [true].
     *)
    val for_all2: (constructor_arg -> 'a -> 'b -> bool) -> 'a t -> 'b t -> bool

    (** [hash t] returns a hash integer of the type constructor [t] which
        carries hashes of its sons. 
     *)
    val hash: int t -> int

    (** Values of type [position] represents a context of pretty-print.
        Distinguishing different contexts allows fine parenthesizing of
        imbricated terms.
     *)
    type position

    (** [parenthesize pos t] returns a boolean indication wether the
        type constructor [t] must be parenthesized in context [pos].
     *)
    val parenthesize: position -> 'a t -> bool

    (** [fprint ppf skip f t] pretty-prints the type constructor [t]
        on the formatter [ppf].  This function may
        - Apply [skip] on each of [t]'s sons.  If [skip x] is true then
          the son [x] does not carry any relevant information and may
          be skipped.
        - Apply [f pos ppf x] on each of [t]'s sons in order to
          pretty-print it.
     *)
    val fprint: formatter -> ('a -> bool) ->
      (constructor_arg -> position -> formatter -> 'a -> unit) -> 'a t -> unit

  end

end



(***************************************************************************)
(** {2 Pretty-print}  *)

(** Printing of constraints may be parametrized by an implementation 
    of the signature [PRINT].  All printing are performed using the module
    [Format] of the Objective Caml standard library.  A general purpose 
    instance implementation is provided by {!Dalton_templates.Print}.
 *)
module type PRINT = sig

  (** The string to be printed in place of ghost variables (i.e. 
      unconstrained anonymous variables), e.g. "_"
   *)
  val ghost: string

  val left_destructor: 'a printer -> formatter -> 'a -> unit
  val right_destructor: 'a printer -> formatter -> 'a -> unit
  val left_destructor_skel: 'a printer -> formatter -> 'a -> unit
  val right_destructor_skel: 'a printer -> formatter -> 'a -> unit

  (** [same_skel printer ppf list] prints a same-skeleton constraint
      involving elements of the list [list].  A printer [printer] is given
      as argument for printing each element of the list.
   *)
  val same_skel: 'a printer -> formatter -> 'a list -> unit

  (** [same_skel printer ppf list] prints an equality involving elements
      of the list [list].  A printer [printer] is given as argument for
      printing each element of the list.
   *)
  val equal: 'a printer -> formatter -> 'a list -> unit

  (** [leq lhs_printer rhs_printer ppf lhs rhs] prints the inequality
      [lhs < rhs] on the formatter [ppf].  Two printers [lhs_printer]
      and [rhs_printer] are provided for printing the left-hand and 
      right-hand sides, respectively.
   *)
      
  val leq: 'a printer -> 'b printer -> formatter -> 'a -> 'b -> unit

  (** [lhs printer ppf list] prints the left-hand side of an inequality,
      consisting in the elements of list the [list].  A printer [printer]
      is given as argument for printing each element of the list. 
   *)
  val lhs: 'a printer -> formatter -> 'a list -> unit

  (** [rhs printer ppf list] prints the right-hand side of an inequality,
      consisting in the elements of list the [list].  A printer [printer]
      is given as argument for printing each element of the list. 
   *)
  val rhs: 'a printer -> formatter -> 'a list -> unit

  (** [cset_begin ppf] is called before printing a constraint set on the
      formatter [ppf].
   *)
  val cset_begin: formatter ->  unit

  (** [cset_end ppf] is called at the end of the printing a constraint set 
      on the formatter [ppf].
   *)
  val cset_end: formatter -> unit

  (** Every constraint [c] of a constraint set is printed on the formatter 
      [ppf] by a call of the form [cset_item printer ppf c] where [printer]
      is a suitable printer for the constraint.
   *)
  val cset_item: 'a printer -> formatter -> 'a -> unit

end



(***************************************************************************)
(** {2 Error report} *)

(** The implementation of the signature [ERROR_REPORT] given to the library
    allows customizing error messages printed when unification, resolution or
    comparison fail.  A general purpose instance implementation is provided
    by {!Dalton_templates.ErrorReport}.
 *)
module type ERROR_REPORT = sig

  (** {3 Unification errors} *)

  (** [unification ppf ~term1 ~term2 ~explanation] reports an unification
      failure of the terms [term1] and [term2].  [explanation] gives a
      short explanation of the reason of the failure, generated itself
      by one of the functions [cycle] or [incompatible].
   *)
  val unification: formatter ->
    term1:printing -> term2:printing -> explanation:printing -> unit

  (** [cycle ppf variable term] prints the explanation of an unification
      failure due to the occur-check.
   *)
  val cycle: formatter ->
    variable:printing -> term:printing -> unit

  (** [cycle ppf ~term1 ~term2] prints the explanation of an unification
      failure due to incompatibles type constructors.
   *)
  val incompatible: formatter ->
    term1:printing -> term2:printing -> unit




  (** {3 Constraints solving errors} *)

  (** [ldestr ppf ~term] tells that the left-destructor has been applied
      on the type term [term] which cannot be so.
   *)
  val ldestr: formatter -> term:printing -> unit

  (** [rdestr ppf ~term] tells that the right-destructor has been applied
      on the type term [term] which cannot be so.
   *)
  val rdestr: formatter -> term:printing -> unit

  (** [inequality ppf ~exp] tells that a constraint is not satisfiable
      because of an inequality described by [expl].
   *)
  val inequality: formatter -> explanation:printing -> unit




  (** {3 Schemes comparison} *)

  (** [incompatible_schemes ppf ~scheme1 ~scheme2 ~explanation] reports
      that schemes [scheme1] and [scheme2] are not comparable, because of
      an unification error.    [explanation] gives a
      short explanation of the reason of the failure, generated itself
      by one of the functions [cycle] or [incompatible] above.
   *)
  val incompatible_schemes: formatter ->
    scheme1:printing -> scheme2:printing -> explanation:printing -> unit

  (** [missing_desc ppf ~scheme ~variable ~term] reports a comparison failure
      due to a variable instatiation.
   *)
  val missing_desc: formatter -> 
    scheme:printing ->  variable:printing -> term:printing -> unit

  (** [missing_desc ppf ~scheme ~variable ~term] reports a comparison failure
      due to a missing inequality.
   *)
  val missing_constraint: formatter ->
    scheme:printing -> constrain:printing -> unit

  (** [missing_desc ppf ~scheme ~variable ~term] reports a comparison failure
      due to a missing constant bound.
   *)
  val missing_bound: formatter ->
    scheme:printing -> constrain:printing -> explanation:printing option -> unit



  (** {3 Minimal instance} *)

  (** [minimal ppf ~scheme ~variablesx] prints a message telling that the
      type scheme [scheme] has no minimal instance.  [variables] prints
      a list of the variables of the scheme which do not have a minimal
      solution.
   *)
  val minimal: formatter -> scheme:printing -> variables:printing -> unit

end



(***************************************************************************)
(** {2 Drawing} *)

(** Graphical representation of schemes is controlled by a module of signature
    [DRAW] giving an implementation of drawing primitives.  This allows 
    performing drawing on a variety of device using appropriate external
    libraries.  An example implementation using the [Graphics] library of the
    Objective Caml system is given in {!Dalton_templates.DrawGraphics}.
*)
module type DRAW = sig

  type window

  val draw_lines: window -> color:color -> lw:int -> (int * int) list -> unit
  val draw_rect: window -> 
      color:color -> lw:int -> x:int -> y:int -> w:int -> h:int -> unit
  val draw_ellipse: window -> 
      color:color -> lw:int -> x:int -> y:int -> rx:int -> ry:int -> unit

  val fill_rect: window -> 
      color:color -> x:int -> y:int -> w:int -> h:int -> unit
  val fill_ellipse: window -> 
      color:color -> x:int -> y:int -> rx:int -> ry:int -> unit
  val fill_poly: window -> color:color -> (int * int) list -> unit

  val draw_text: window -> color:color -> x:int -> y:int -> string -> unit
  val text_size: window -> string -> int * int

  val draw_vertical_dots: 
      window -> color:color -> x:int -> y:int -> y':int -> unit
  val draw_horizontal_dots: 
      window -> color:color -> x:int -> x':int -> y:int -> unit

end
