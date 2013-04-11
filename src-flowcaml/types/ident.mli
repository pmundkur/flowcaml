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

(* $Id: ident.mli,v 1.2 2003/06/26 13:32:56 simonet Exp $ *)
(* Ident: Identifiers (unique names) *)

type t

val create: string -> t
val create_persistent: string -> t
val rename: t -> t
val name: t -> string
val unique_name: t -> string
val persistent: t -> bool
val equal: t -> t -> bool
        (* Compare identifiers by name. *)      
val same: t -> t -> bool
        (* Compare identifiers by binding location.
           Two identifiers are the same either if they are both
           non-persistent and have been created by the same call to
           [new], or if they are both persistent and have the same
           name. *)
val hide: t -> t
        (* Return an identifier with same name as the given identifier,
           but stamp different from any stamp returns by new.
           When put in a 'a tbl, this identifier can only be looked
           up by name. *)

val make_global: t -> unit
val global: t -> bool

val binding_time: t -> int
val current_time: unit -> int
val set_current_time: int -> unit

val print: Format.formatter -> t -> unit

type 'a tbl
        (* Association tables from identifiers to type 'a. *)

val empty: 'a tbl
val add: t -> 'a -> 'a tbl -> 'a tbl
val find_same: t -> 'a tbl -> 'a
val find_name: string -> 'a tbl -> 'a
val remove: t -> 'a tbl -> 'a tbl
val map: ('a -> 'b) -> 'a tbl -> 'b tbl
val iter: ('a -> unit) -> 'a tbl -> unit
val fold: (t -> 'a -> 'b -> 'b) -> 'a tbl -> 'b -> 'b

val fprint: Format.formatter -> t -> unit

val stamp: t -> int
val create_unsafe: string -> int -> t
