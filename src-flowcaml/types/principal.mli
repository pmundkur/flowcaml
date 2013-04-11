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

(* $Id: principal.mli,v 1.2 2003/06/26 13:32:57 simonet Exp $ *)
(* Principal: the lattice of principals *)

open Parsetree

type lattice

val create: unit -> lattice

val translate_into: lattice -> flows_declaration -> unit
val translate: flows_declaration -> lattice
val merge_into: lattice -> lattice -> unit

val included: lattice -> lattice -> (string * string) option

val leq: lattice -> string -> string -> bool

val fprint: Format.formatter -> lattice -> unit
val draw: lattice -> int -> int -> int * int

