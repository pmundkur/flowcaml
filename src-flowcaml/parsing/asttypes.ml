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

(* $Id: asttypes.ml,v 1.5 2003/06/26 13:32:49 simonet Exp $ *)
(* Asttypes: Auxiliary a.s.t. types used by parsetree and typedtree. *)

type constant =
    Const_int of int
  | Const_char of char
  | Const_string of string
  | Const_float of string
  | Const_charray of string * Location.t
	(* ACHTUNG ! Laisser en dernier pour la compatibilité avec
	   l'AST d'Ocaml ! *)

type rec_flag = Nonrecursive | Recursive | Default

type direction_flag = Upto | Downto

type private_flag = Private | Public

type mutable_flag = Immutable | Mutable

type try_case = Throw | Propagate of Location.t
    (* location of propagate keyword, required for OCaml code generation *)

type virtual_flag = Virtual | Concrete

type label = string
