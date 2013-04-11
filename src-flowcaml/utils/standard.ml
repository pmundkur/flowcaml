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

(* $Id: standard.ml,v 1.2 2003/06/26 13:33:00 simonet Exp $
   standard.ml: Miscellaneous functions
 *)

(* This module provides some miscellaneous functions that are missing
   is the standard library. *)



(* -----------------------------------------------------------------------
   Operations on lists
 *)

(* [filter_map] expects a ``constructive predicate'' [p] and a list
   [l]. A ``constructive predicate'' is a function which, given an
   element [x], returns either another [x'], or nothing. [filter_map p
   l] returns the list obtained by applying [p] to each element of
   [l], and gathering any elements returned by [p]. [filter_map] is
   thus a combination of [filter] and [map]. *)

let filter_map f list =
  let rec map_filter_rec = function
      [] -> []
    | t :: q ->
	match f t with
	  None -> map_filter_rec q
	| Some x -> x :: map_filter_rec q
  in
  map_filter_rec list

let filter_map2 f list =
  let rec map_filter_rec l1 l2 =
    match l1, l2 with
      [], [] -> []
    | [], _ :: _ | _ :: _, [] -> invalid_arg "Standard.filter_map2"
    | t1 :: q1, t2 :: q2 ->
	match f t1 t2 with
	  None -> map_filter_rec q1 q2
	| Some x -> x :: map_filter_rec q1 q2
  in
  map_filter_rec list


let sort_unique cmp list =
  let rec aux accu = function
      [] -> accu
    | x :: [] -> x :: accu
    | x :: ((y :: _) as tl) when cmp x y = 0 -> aux accu tl
    | x :: tl -> aux (x :: accu) tl
  in
  aux [] (List.sort (fun x y -> cmp y x) list)

let sort_merge cmp merge list =
  let rec aux accu = function
      [] -> accu
    | x :: [] -> x :: accu
    | x :: y :: tl when cmp x y = 0 -> aux accu ((merge x y) :: tl)
    | x :: tl -> aux (x :: accu) tl
  in
  aux [] (List.sort (fun x y -> cmp y x) list)

let classes cmp e = 
  let e' = List.sort cmp e in
  match e' with
    [] -> []
  | hd :: tl ->
      let x, curr, cc =
	List.fold_left (fun (y, curr, cc) x ->
	  if cmp x y = 0 then (x, y :: curr, cc)
	  else (x, [], (y :: curr) :: cc)
        ) (hd, [], []) tl  
      in
      (x :: curr) :: cc


(* [iter2by2 f list] applies the function [f] on each pair of successive 
   elements in [list] *)

let iter2by2 f list =
  let rec iter2by2_rec = function
      [] | _ :: [] -> ()
    | x :: ((y :: _) as tl) -> f x y; iter2by2_rec tl
  in
  iter2by2_rec list



(* [mem cmp x list] is true if and only if [x] is equal
   to an element of [list] (using the comparison function [cmp]. *)

let mem cmp x list =
  let rec mem_rec = function
      [] -> false
    | y :: _ when cmp x y = 0 -> true
    | _ :: q -> mem_rec q
  in
  mem_rec list



(* [merge] *)

let merge cmp merge list =
  let rec assoc_merge_rec = function
      [] | _ :: [] as list -> list
    | x :: y :: q when cmp x y = 0 -> assoc_merge_rec ((merge x y) :: q)
    | x :: q -> x :: assoc_merge_rec q
  in
  assoc_merge_rec (List.sort cmp list)



(* [map3] is analogous to [List.map2], with three lists. *)

let rec map3 f l1 l2 l3 =
  match l1, l2, l3 with
    [], [], [] -> []
  | x1 :: q1, x2 :: q2, x3 :: q3 ->
      (f x1 x2 x3) ::  (map3 f q1 q2 q3)
  | _ -> invalid_arg "map3"



(* [iter3] is analogous to [List.iter2], with three lists. *)

let rec iter3 f l1 l2 l3 =
  match l1, l2, l3 with
    [], [], [] -> ()
  | x1 :: q1, x2 :: q2, x3 :: q3 ->
      f x1 x2 x3;
      iter3 f q1 q2 q3
  | _ -> invalid_arg "Standard.iter3"



(* [iter4] is analogous to [List.iter4], with four lists. *)

let rec iter4 f l1 l2 l3 l4 =
  match l1, l2, l3, l4 with
    [], [], [], [] -> ()
  | x1 :: q1, x2 :: q2, x3 :: q3, x4 :: q4 ->
      f x1 x2 x3 x4;
      iter4 f q1 q2 q3 q4
  | _ -> invalid_arg "Standard.iter4"



(* [iter5] is analogous to [List.iter2], with five lists. *)

let rec iter5 f l1 l2 l3 l4 l5 =
  match l1, l2, l3, l4, l5 with
    [], [], [], [], [] -> ()
  | x1 :: q1, x2 :: q2, x3 :: q3, x4 :: q4, x5 :: q5 ->
      f x1 x2 x3 x4 x5;
      iter5 f q1 q2 q3 q4 q5
  | _ -> invalid_arg "Standard.iter5"



(* [for_all3] is analogous to [List.for_all2], with three lists. *)

let rec for_all3 f l1 l2 l3 =
  match l1, l2, l3 with
    [], [], [] -> true
  | x1 :: q1, x2 :: q2, x3 :: q3 ->
      (f x1 x2 x3) && (for_all3 f q1 q2 q3)
  | _ -> invalid_arg "Standard.for_all3"




(* [iter_square f [x1; ...; xn] [y1; ...; ym]] applies [f] on each
   pair [xi, yj] for $1 \leq i \leq n$ and $1 \leq j \leq m$. *)

let iter_square f list1 list2 =

  List.iter (function x -> List.iter (f x) list2) list1


(* [list_max list] returns the greatest element (w.r.t. <) of the list [list]
 *)
let list_max = function
    [] -> invalid_arg "Standard.list_max"
  | hd :: tl ->
      let rec list_max_rec accu = function
	  [] -> accu
	| hd :: tl -> list_max_rec (max hd accu) tl
      in
      list_max_rec hd tl

(* [list_min list] returns the lowest element (w.r.t. <) of the list [list]
 *)
let list_min = function
    [] -> invalid_arg "Standard.list_min"
  | hd :: tl ->
      let rec list_min_rec accu = function
	  [] -> accu
	| hd :: tl -> list_min_rec (min hd accu) tl
      in
      list_min_rec hd tl
