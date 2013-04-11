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

(* $Id: dalton_aux.mli,v 1.2 2003/06/26 13:32:46 simonet Exp $ *)


(** Auxiliary definitions

    This module define several auxiliary datatypes that are useful for the
    description of the ground algebra.  They are also used internally by
    the solver.
 *)

open Format



(** Pretty-printing in the library is performed by the [Format] module.
    Therefore a {i printer} of values of type ['a] may be viewed as a 
    function of type ['a printer].
*)
type 'a printer = formatter -> 'a -> unit

(** Similarly, the printing of some message on a formatter may be abstractly 
    represented by a function of type [printing].
 *)
type printing = formatter -> unit

(** For drawing purposes, a color is represented by a simple integer, 
    as in the [Graphics] module of the Objective Caml standard library.
 *)
type color = int



(***************************************************************************)
(** {2 Kinds} *)

(** In the term algebra considered by the solver, terms may have one of 
    the following kinds:
    - [Katom] for atoms,
    - [Ktype] for type,
    - [Krow k] for rows whose elements have kind [k].
 *)
type kind =
    Katom
  | Ktype
  | Krow of kind



(** Basic operations on kinds are provided by the module [Variance].
 *)
module Kind : sig

  (** [atomic k] tests whether the kind [k] is atomic, i.e. is [Katom] 
      or [Krow Katom] or [Krow (Krow Katom)] etc. 
   *)
  val atomic: kind -> bool

  (** [rows k] counts the number of [Row] in the kind [k].
      For instance [rows Katom] and [rows Ktype] return [0], while
      [rows (Krow Katom)] and [rows (Krow (Krow Ktype))] return
      respectively [1] and [2].
   *)
  val rows: kind -> int

  (** [fprint ppf k] prints the kind [k] on the formatter [ppf].
   *)
  val fprint: formatter -> kind -> unit

end



(***************************************************************************)
(** {2 Variances} *)

(** A variance is one of the three elements [Covariant], [Contravariant]
    and [Invariant].
 *)
type variance =
    Covariant
  | Contravariant
  | Invariant


(** Basic operations on variances are provided by the module [Variance].
 *)
module Variance : sig

  (** [leq v1 v2] tests whether the variance [v1] is less than or equal to
      the variance [v2] in the usual order on variance (which is the smallest 
      order such that [Covariant] and [Contravariant] are less than [Invariant].
   *)
  val leq: variance -> variance -> bool

  (** [combine v1 v2] calculates the combination of two variances.
   *)
  val combine: variance -> variance -> variance

  (** [to_string v] gives a string representation of the variance [v].
   *)
  val to_string: variance -> string

  (** [fprint ppf v] prints the variance [v] on the formatter [ppf],
      using one of the three symbols "+", "-" and "=".
   *)      
  val fprint: formatter -> variance -> unit
	
  (** [fprint_name ppf v] prints the variance [v] on the formatter [ppf],
      using its litteral name (i.e. "covariant", "contravariant" 
      or "invariant").
   *)
  val fprint_name: formatter -> variance -> unit

end



(***************************************************************************)
(** {2 Contructor arguments} *)

(** Signatures of type constructors are specified by giving for each 
    argument a record of type [constructor_arg].
 *)
type constructor_arg =
    { variance: variance;
      (** The variance of the argument. *)
      kind: kind;
      (** The kind of the argument. *)
      ldestr: bool;
      (** A boolean setting whether left destructor decomposes on this 
	  argument. *)
      rdestr: bool
      (** A boolean setting whether right destructor decomposes on this 
	  argument. *)
    } 
