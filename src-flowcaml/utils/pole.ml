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

(* $Id: pole.ml,v 1.2 2003/06/26 13:33:00 simonet Exp $ *)
(* Pole: a four element lattice *)

open Dalton_variance



type t = bool * bool

let bot = false, false
let pos = true, false
let neg = false, true
let top = true, true


let leq (a1, b1) (a2, b2) = (a1 <= a2) && (b1 <= b2)
let union (a1, b1) (a2, b2) = (a1 || a2, b1 || b2)



(* Terms. *)

type 'a term = 
    Term of 'a
  | Opp of 'a
  | And of 'a * 'a
  | Combine of 'a * 'a

let eval f = function
    Term x -> f x
  | Opp x ->
      let (a, b) = f x in b, a
  | And (x1, x2) -> 
      let a1, b1 = f x1
      and a2, b2 = f x2 in
      (a1 && a2, b1 && b2)
  | Combine (x1, x2) ->
      let a1, b1 = f x1
      and a2, b2 = f x2 in
      (a1 && a2 || b1 && b2, a1 && b2 || b1 && a2)

let iter f = function
    Term x | Opp x -> f x
  | And (x1, x2) | Combine (x1, x2) -> f x1; f x2


(* Conversions. *)

exception Bot
exception Top
	
let of_variance = function
    Covariant -> pos
  | Contravariant -> neg
  | Invariant -> top

let to_variance = function
    false, false -> raise Bot
  | true, false -> Covariant
  | false, true -> Contravariant
  | true, true -> Invariant

let of_bool = function
    false -> false, true
  | true -> true, false

let of_true = function
    false -> false, false
  | true -> true, false

let to_bool = function
    false, false -> raise Bot
  | true, true -> raise Top
  | true, false -> true
  | false, true -> false
