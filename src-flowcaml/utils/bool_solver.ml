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

(* $Id: bool_solver.ml,v 1.2 2003/06/26 13:32:59 simonet Exp $ *)
(* Bool_solver *)



type 'a bool_term =
    Term of 'a
  | And of 'a * 'a



include Lattice_solver.Make (struct

  type t = bool

  let bot = false
  let leq = (<=)
  let union = (||)

  type 'a term = 'a bool_term


let eval f = function
    Term x -> f x
  | And (x1, x2) -> (f x1) && (f x2)

let iter f = function
    Term x -> f x
  | And (x1, x2) -> f x1; f x2

end)
