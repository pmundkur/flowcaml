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

(* $Id: pole.mli,v 1.2 2003/06/26 13:33:00 simonet Exp $ *)
(* Pole: a four element lattice *)



type t

val bot: t
val pos: t
val neg: t
val top: t

val leq: t -> t -> bool
val union: t -> t -> t



type 'a term = 
    Term of 'a
  | Opp of 'a
  | And of 'a * 'a
  | Combine of 'a * 'a

val eval: ('a -> t) -> 'a term -> t
val iter: ('a -> unit) -> 'a term -> unit



exception Bot
exception Top

val of_variance: Dalton_variance.t -> t
val to_variance: t -> Dalton_variance.t
val to_bool: t -> bool
val of_bool: bool -> t
val of_true: bool -> t
