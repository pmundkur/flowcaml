(**************************************************************************)
(*                                                                        *)
(*                               Flow Caml                                *)
(*                                                                        *)
(*          Vincent Simonet, Projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*  Copyright 2002, 2003 Institut National de Recherche en Informatique   *)
(*  et en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with the   *)
(*  special exception on linking described in file ../LICENSE.            *)
(*                                                                        *)
(*  Author contact: Vincent.Simonet@inria.fr                              *)
(*  Software page: http://cristal.inria.fr/~simonet/soft/flowcaml/        *)
(*                                                                        *)
(**************************************************************************)

(* $Id: flowperv.mli,v 1.5 2003/06/26 13:32:51 simonet Exp $ *)



(***************************************************************************)
(** {2 Strings} *)

type charray = string

val (^^) : charray -> charray -> charray
val ($$) : string -> int -> char
val charray_of_string : string -> charray
val string_of_charray : charray -> string



(***************************************************************************)
(** {2 Exceptions} *)

val _propagate_: (unit -> 'a) -> exn -> 'b
val _try_finally_: (unit -> 'a) -> (unit -> 'b) -> 'a
val _catchable_: exn -> bool
