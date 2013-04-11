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

(* $Id: dalton_lib.mli,v 1.2 2003/06/26 13:32:47 simonet Exp $ *)

(** Library

    This module provide some library functions used by Dalton.
 *)



(***************************************************************************)
(** {2 Standard library add-ons} *)

module Standard : sig

  (** [filter_map] expects a ``constructive predicate'' [p] and a list
      [l]. A ``constructive predicate'' is a function which, given an
      element [x], returns either another [x'], or nothing. [filter_map p
      l] returns the list obtained by applying [p] to each element of
      [l], and gathering any elements returned by [p]. [filter_map] is
      thus a combination of [filter] and [map]. 
   *)
  val filter_map: ('a -> 'b option) -> 'a list -> 'b list

  (** [classes cmp e] returns the list of lists of equals elements of [e],
      according to the total order [cmp].
   *)
  val classes: ('a -> 'a -> int) -> 'a list -> 'a list list

  (** Given a function [compare_elt] ordering values of type ['a],
      [compare_list compare_elt] returns a function ordering list of
      type ['a list].
   *)
  val compare_list: ('a -> 'a -> int) -> ('a list -> 'a list -> int)

end



(***************************************************************************)
(** {2 Union-find} *)

(** The module [Proxy] defines data structures which support a basic
    union/find algorithm. These will be useful in every implementation
    of unification. 
 *)
module Proxy : sig

  (** A [proxy] is a cell which represents a point in some graph. It has
      physical identity. Its contents simply consist of a mutable [link],
      which leads either directly to a [descriptor] of unknown type ['a],
      or to another [proxy], in which case the first proxy is to be
      viewed as an alias for the second one.
   *)
  type 'a t

  (** [create desc] returns a fresh proxy for the specified descriptor. 
   *)
  val create: 'a -> 'a t

  (** [repr proxy] returns a proxy's representative, that is, a canonical
      proxy for which [proxy] is an alias. It internally performs path
      compression. 
   *)
  val repr: 'a t -> 'a t

  (** [desc proxy] returns the descriptor associated with the proxy [proxy]. 
   *)
  val desc: 'a t -> 'a

  (** [linksto proxy1 proxy2] makes [proxy1] an alias for [proxy2]. 
      [proxy1]'s descriptor becomes no longer reachable through [proxy1].
   *)
  val linksto: 'a t -> 'a t -> unit

  (** [principal proxy] returns [true] for exactly one member of the
      equivalence class. 
   *)
  val principal: 'a t -> bool

end



(***************************************************************************)
(** {2 Marks} *)

module type M = sig

  type t

  val mark: t -> bool
  val unmark: t -> unit

end



module MList (X: M) : sig

  type elt = X.t
  type t = elt list

  val iter: (elt -> unit) -> t -> unit
  val filter: (elt -> bool) -> t -> t
  val clean: t -> t
  val union_except: t -> t -> t -> t
  val union: t -> t -> t
  val iter_diff: (elt -> unit) -> t -> t -> unit

end



module Mark : sig

  type t

  val create: unit -> t
  val same: t -> t -> bool
  val none: t

end



(** {2 Lists of markable elements} *)

module type MARKABLE = sig

  type t

  val get: t -> Mark.t
  val set: t -> Mark.t -> unit

end



module Marked_list (X: MARKABLE) : sig

  type elt = X.t
  type t = elt list


  val iter: (elt -> unit) -> t -> unit
  (* val exists: (elt -> bool) -> t -> bool*)
  (* val fold: (elt -> 'a -> 'a) -> t -> 'a -> 'a*)
  val filter: (elt -> bool) -> t -> t
  val clean: t -> t
  (* val is_clean: t -> bool*)
  val union_except: t -> t -> t -> t
  val union: t -> t -> t
  val iter_diff: (elt -> unit) -> t -> t -> unit

end



(***************************************************************************)
(** {2 Hash-consing} *)

module type HASHABLE = sig

  (* The type of elements on which hash-consing operates. *)

  type t

  (* [t] must be a "hashed type", i.e. the client must provide two
     functions [equal] and [hash] such that if [equal x y] is [true]
     then [hash x] and [hash y] are equal.  However if there is no [y]
     such that [equal x y] holds then [hash x] may raise the exception
     [No_hash]. *)

  exception No_hash

  val equal: t -> t -> bool
  val hash: t -> int

  (* The [unify] function will be applied on pairs of equal elements.  
     This function must not change the result of [equal] and [hash] functions
     on its arguments. *)

  val unify: t -> t -> unit

end



module Hash_consing (X : HASHABLE) : sig

  (* [f iter i] performs the hash-consing.  [iter f] must apply [f]
     succesively on every elements.  Be careful: [unify] will be
     called during the walk.  
   *)
  val f: ((X.t -> unit) -> unit) -> int -> unit

end
