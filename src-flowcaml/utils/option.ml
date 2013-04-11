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

(* $Id: option.ml,v 1.2 2003/06/26 13:33:00 simonet Exp $ *)
(* Option: Operations on the option type. *)

(* This module provides some useful operations dealing with the option type
   of the standard library. *)



(* [test x] is true if [x] is [Some _]. *)

let test = function
    None -> false
  | Some _ -> true



(* [map f None] returns [None] while [map f (Some x)] returns [Some (f x)]. *)

let map f = function
    None -> None
  | Some x -> Some (f x)



(* [iter f opt] applies [f] on the content of the option [opt], if any. *)

let iter f = function
    None -> ()
  | Some x -> f x



let iter2 f opt1 opt2 =
  match opt1, opt2 with
    None, None -> ()
  | Some x1, Some x2 -> f x1 x2
  | None, Some _ | Some _, None -> invalid_arg "Option.iter2"


(* [default x opt] returns [y] if [opt] is [Some y], [x] otherwise. *)

let default x = function
    None -> x
  | Some y -> y
