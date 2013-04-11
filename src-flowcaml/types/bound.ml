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

(* $Id: bound.ml,v 1.2 2003/06/26 13:32:55 simonet Exp $ *)
(* Bound: *)

open Format


(*

let union bnd1 bnd2 =
  match bnd1, bnd2 with
    Bnd_none, _ | _, Bnd_none -> 
      Bnd_none
  | Bnd_levels set1, Bnd_levels set2 -> 
      Bnd_levels (Level.Set.union set1 set2)

let compare bnd1 bnd2 =
  match bnd1, bnd2 with
    Bnd_none, Bnd_none -> 0
  | Bnd_none, Bnd_levels _ -> 1
  | Bnd_levels _, Bnd_none -> -1
  | Bnd_levels set1, Bnd_levels set2 ->
      Level.Set.compare set1 set2

let fprint ppf = function
    Bnd_none -> fprintf ppf "#"
  | Bnd_levels set -> Level.Set.fprint ppf set

*)
