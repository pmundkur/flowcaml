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

(* $Id: standard.mli,v 1.2 2003/06/26 13:33:00 simonet Exp $
   standard.mli: Miscellaneous functions
 *)

(* This module provides some miscellaneous functions that are missing
   is the standard library. *)



(* -----------------------------------------------------------------------
   Operations on lists
 *)

(* [filter_map] expects a ``constructive predicate'' [p] and a list
   [l]. A ``constructive predicate'' is a function which, given an
   element [x], returns either another [x'], or nothing. [filter_map p
   l] returns the list obtained by applying [p] to each element of
   [l], and gathering any elements returned by [p]. [filter_map] is
   thus a combination of [filter] and [map]. *)

val filter_map: ('a -> 'b option) -> 'a list -> 'b list

val filter_map2: ('a -> 'b -> 'c option) -> 'a list -> 'b list -> 'c list

val sort_merge: ('a -> 'a -> int) -> ('a -> 'a -> 'a) -> 'a list -> 'a list

val classes: ('a -> 'a -> int) -> 'a list -> 'a list list 

(* [iter2by2 f list] applies the function [f] on each pair of successive 
   elements in [list] *)

val iter2by2: ('a -> 'a -> unit) -> 'a list -> unit

(* [mem cmp x list] is true if and only if [x] is equal
   to an element of [list] (using the comparison function [cmp]. *)

val mem: ('a -> 'a -> int) -> 'a -> 'a list -> bool

(* [merge] *)

val merge: ('a -> 'a -> int) -> ('a -> 'a -> 'a) -> 'a list -> 'a list

(* [map3] is analogous to [List.map2], with three lists. *)

val map3: ('a -> 'b -> 'c -> 'd) -> 'a list -> 'b list -> 'c list -> 'd list

(* [iter3] is analogous to [List.iter2], with three lists. *)

val iter3: ('a -> 'b -> 'c -> unit) -> 'a list -> 'b list -> 'c list -> unit

(* [iter4] is analogous to [List.iter2], with four lists. *)

val iter4: ('a -> 'b -> 'c -> 'd -> unit)
    -> 'a list -> 'b list -> 'c list -> 'd list -> unit

(* [iter5] is analogous to [List.iter2], with five lists. *)

val iter5: ('a -> 'b -> 'c -> 'd -> 'e -> unit)
    -> 'a list -> 'b list -> 'c list -> 'd list -> 'e list -> unit

(* [for_all3] is analogous to [List.for_all2], with three lists. *)

val for_all3: ('a -> 'b -> 'c -> bool) -> 'a list -> 'b list -> 'c list -> bool

(* [iter_square f [x1; ...; xn] [y1; ...; ym]] applies [f] on each
   pair [xi, yj] for $1 \leq i \leq n$ and $1 \leq j \leq m$. *)

val iter_square: ('a -> 'b -> unit) -> 'a list -> 'b list -> unit

(* [list_max list] returns the greatest element (w.r.t. <) of the list [list]
 *)
val list_max: 'a list -> 'a

(* [list_min list] returns the greatest element (w.r.t. <) of the list [list]
 *)
val list_min: 'a list -> 'a

