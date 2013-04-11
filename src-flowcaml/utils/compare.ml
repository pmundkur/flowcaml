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

(* $Id: compare.ml,v 1.2 2003/06/26 13:32:59 simonet Exp $ *)
(* Compare: Manipulating "compare" functions *)



(* A total order $\leq$ on the elements of a type ['a] is usually represented 
   by a [compare] function of type ['a -> 'a -> int] such that [compare x y]
   returns:
   - zero if $x = y$ (i.e. $x \leq y$ and $y \leq x$,
   - a negative integer if $x < y$,
   - a positive integer if $x > y$.
 *)

type 'a t = 'a -> 'a -> int



(* Given an order [compare] on elements of type ['a], [option compare]
   is the corresponding order on elements of type ['a option]
   (considering [None] as the minimal element. *)

let option compare opt1 opt2 =
  match opt1, opt2 with
    None, None -> 0
  | None, Some _ -> -1
  | Some _, None -> 1
  | Some x1, Some x2 -> compare x1 x2



(* [pair compare1 compare2] computes a lexicographical product of the
   two orders [compare1] and [compare2]. *)

let pair compare1 compare2 =
  fun (x1, x2) (y1, y2) ->
    let c1 = compare1 x1 y1 in
    if c1 <> 0 then c1 else compare2 x2 y2


(* [pair_rev compare1 compare2] computes a lexicographical product of the
   two orders [compare2] and [compare1]. *)
let pair_rev compare1 compare2 =
  fun (x1, x2) (y1, y2) ->
    let c2 = compare2 x2 y2 in
    if c2 <> 0 then c2 else compare1 x1 y1



(* [pair1 compare1] orders pairs considering only their first component. *)

let pair1 compare1 =
  fun (x1, _) (y1, _) -> compare1 x1 y1



(* [triple] computes a lexicographical product of three orders. *)

let triple compare1 compare2 compare3 =
  fun (x1, x2, x3) (y1, y2, y3) ->
    let c1 = compare1 x1 y1 in
    if c1 <> 0 then c1 else begin
      let c2 = compare2 x2 y2 in
      if c2 <> 0 then c2 else compare3 x3 y3
    end



(* [list] extend lexicographically an order on lists. *)

let list compare_elt =
  let rec list_rec list1 list2 =
    match list1, list2 with
      [], [] -> 0
    | [], _ :: _ -> -1
    | _ :: _, [] -> 1
    | t1 :: q1, t2 :: q2 -> 
	let c = compare_elt t1 t2 in
	if c <> 0 then c else list_rec q1 q2
  in
  list_rec



(* [opp] *)

let opp compare =
  fun x1 x2 -> compare x2 x1
