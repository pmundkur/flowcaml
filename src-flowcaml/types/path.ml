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

(* $Id: path.ml,v 1.2 2003/06/26 13:32:57 simonet Exp $ *)
(* Path: Access paths *)

type t =
    Pident of Ident.t
  | Pdot of t * string * int
  | Papply of t * t

let nopos = -1

let rec same p1 p2 =
  match (p1, p2) with
    (Pident id1, Pident id2) -> Ident.same id1 id2
  | (Pdot(p1, s1, pos1), Pdot(p2, s2, pos2)) -> s1 = s2 && same p1 p2
  | (Papply(fun1, arg1), Papply(fun2, arg2)) ->
       same fun1 fun2 && same arg1 arg2
  | (_, _) -> false

let rec isfree id = function
    Pident id' -> Ident.same id id'
  | Pdot(p, s, pos) -> isfree id p
  | Papply(p1, p2) -> isfree id p1 || isfree id p2

let rec binding_time = function
    Pident id -> Ident.binding_time id
  | Pdot(p, s, pos) -> binding_time p
  | Papply(p1, p2) -> max (binding_time p1) (binding_time p2)

let rec name = function
    Pident id -> Ident.name id
  | Pdot(p, s, pos) -> name p ^ "." ^ s
  | Papply(p1, p2) -> name p1 ^ "(" ^ name p2 ^ ")"

let compare = Pervasives.compare  (* TEMPORARY *)

let hash = Hashtbl.hash (* TEMPORARY *)

let ident_pervasive = Ident.create_persistent "Pervasives"

let rec fprint ppf = function
    Pident i -> Ident.fprint ppf i
  | Pdot(Pident id, s, pos) when Ident.same id ident_pervasive ->
      Format.fprintf ppf "%s" s
  | Pdot (p, s, _) -> Format.fprintf ppf "%a.%s" fprint p s
  | Papply (p1, p2) -> Format.fprintf ppf "%a(%a)" fprint p1 fprint p2
