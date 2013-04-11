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

(* $Id: option.mli,v 1.2 2003/06/26 13:33:00 simonet Exp $ *)
(* Option: Operations on the option type. *)

(* This module provides some useful operations dealing with the option type
   of the standard library. *)


(* [test x] is true if [x] is [Some _]. *)

val test: 'a option -> bool

(* [map f None] returns [None] while [map f (Some x)] returns [Some (f x)]. *)

val map: ('a -> 'b) -> 'a option -> 'b option

(* [iter f opt] applies [f] on the content of the option [opt], if any. *)

val iter: ('a -> unit) -> 'a option -> unit

val iter2: ('a -> 'b -> unit) -> 'a option -> 'b option -> unit

(* [default x opt] returns [y] if [opt] is [Some y], [x] otherwise. *)

val default: 'a -> 'a option -> 'a
