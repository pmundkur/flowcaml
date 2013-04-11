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

(* $Id: dalton_aux.ml,v 1.2 2003/06/26 13:32:46 simonet Exp $ *)

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



module Kind = struct

  (** [atomic k] tests whether the kind [k] is atomic, i.e. is [Katom] 
      or [Krow Katom] or [Krow (Krow Katom)] etc. 
   *)
  let rec atomic = function
      Katom -> true
    | Ktype -> false
    | Krow k -> atomic k

  (** [rows k] counts the number of [Row] in the kind [k].
      For instance [rows Katom] and [rows Ktype] return [0], while
      [rows (Krow Katom)] and [rows (Krow (Krow Ktype))] return
      respectively [1] and [2].
   *)
  let rec rows = function
      Katom | Ktype -> 0
    | Krow k -> 1 + rows k

  (** [fprint ppf k] prints the kind [k] on the formatter [ppf].
   *)
  let rec fprint ppf = function
      Katom -> fprintf ppf "Atom"
    | Ktype -> fprintf ppf "Type"
    | Krow k -> fprintf ppf "Row[%a]" fprint k

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
module Variance = struct

  (** [leq v1 v2] tests whether the variance [v1] is less than or equal to
      the variance [v2] in the usual order on variance (which is the smallest 
      order such that [Covariant] and [Contravariant] are less than [Invariant].
   *)
  let leq v1 v2 =
    match v1, v2 with
      (Covariant | Contravariant | Invariant), Invariant
    | Covariant, Covariant | Contravariant, Contravariant -> true
    | Contravariant, Covariant | Covariant, Contravariant
    | Invariant, (Covariant | Contravariant) -> false

  (** [combine v1 v2] calculates the product of two variances.
   *)
  let combine v1 v2 =
    match v1, v2 with
      Covariant, _ -> v2
    | _, Covariant -> v1
    | Invariant, _ | _, Invariant -> Invariant
    | Contravariant, Contravariant -> Covariant

  (** [to_string v] gives a string representation of the variance [v].
   *)
  let to_string = function
      Covariant -> "+"
    | Contravariant -> "-"
    | Invariant -> "="

  (** [fprint ppf v] prints the variance [v] on the formatter [ppf],
      using one of the three symbols "+", "-" and "=".
   *)      
  let fprint ppf = function
      Covariant -> fprintf ppf "+"
    | Contravariant -> fprintf ppf "-"
    | Invariant -> fprintf ppf "="
	
  (** [fprint_name ppf v] prints the variance [v] on the formatter [ppf],
      using its litteral name (i.e. "covariant", "contravariant" 
      or "invariant").
   *)
  let fprint_name ppf = function
      Covariant -> fprintf ppf "covariant"
    | Contravariant -> fprintf ppf "contravariant"
    | Invariant -> fprintf ppf "invariant"

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
