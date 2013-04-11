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

(* $Id: variance_solver.ml,v 1.3 2003/06/26 13:33:00 simonet Exp $ *)
(* Variance_solver *)

open Dalton_aux



type 'a variance_term =
    Term of 'a
  | Opp of 'a
  | Combine of 'a * 'a



include Lattice_solver.Make (struct

  type t = bool * bool

  let bot = false, false
  let leq (a1, b1) (a2, b2) = (a1 <= a2) && (b1 <= b2)
  let union (a1, b1) (a2, b2) = (a1 || a2, b1 || b2)
 
  type 'a term = 'a variance_term 

  let eval f = function
      Term x -> f x
    | Opp x ->
	let (a, b) = f x in b, a
    | Combine (x1, x2) ->
	let a1, b1 = f x1
	and a2, b2 = f x2 in
	(a1 && a2 || b1 && b2, a1 && b2 || b1 && a2)

  let iter f = function
      Term x | Opp x -> f x
    | Combine (x1, x2) -> f x1; f x2

end)



let of_variance variance = 
  create begin match variance with
    Covariant -> (true, false)
  | Contravariant -> (false, true)
  | Invariant -> (true, true)
  end

let to_variance v =
  match eval v with
    false, false -> Invariant
  | true, false -> Covariant
  | false, true -> Contravariant
  | true, true -> Invariant
