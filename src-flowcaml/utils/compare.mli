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

(* $Id: compare.mli,v 1.2 2003/06/26 13:32:59 simonet Exp $ *)
(* Compare: Manipulating "compare" functions *)



(* A total order $\leq$ on the elements of a type ['a] is usually represented 
   by a [compare] function of type ['a -> 'a -> int] such that [compare x y]
   returns:
   - zero if $x = y$ (i.e. $x \leq y$ and $y \leq x$,
   - a negative integer if $x < y$,
   - a positive integer if $x > y$.
 *)

type 'a t = 'a -> 'a -> int



(* Given an order [compare] on elements of type ['a], [option compare]
   is the corresponding order on elements of type ['a option]
   (considering [None] as the minimal element. *)

val option: 'a t -> 'a option t

(* [pair compare1 compare2] computes a lexicographical product of the
   two orders [compare1] and [compare2]. *)

val pair: 'a t -> 'b t -> ('a * 'b) t

(* [pair_rev compare1 compare2] computes a lexicographical product of the
   two orders [compare2] and [compare1]. *)

val pair_rev: 'a t -> 'b t -> ('a * 'b) t

(* [pair1 compare1] orders pairs considering only their first component. *)

val pair1: 'a t -> ('a * 'b) t

(* [triple] computes a lexicographical product of three orders. *)

val triple: 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t

(* [list] extend lexicographically an order on lists. *)

val list: 'a t -> 'a list t

(* [opp] *)

val opp: 'a t -> 'a t
