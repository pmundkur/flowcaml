(**************************************************************************)
(*                                                                        *)
(*                                  Dalton                                *)
(*                      an efficient implementation of                    *)
(*                 type inference with structural subtyping               *)
(*                                                                        *)
(*          Vincent Simonet, Projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*  Copyright 2002, 2003 Institut National de Recherche en Informatique   *)
(*  et en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with the   *)
(*  special exception on linking described in file LICENSE.               *)
(*                                                                        *)
(*  Author contact: Vincent.Simonet@inria.fr                              *)
(*  Software page: http://cristal.inria.fr/~simonet/soft/dalton/          *)
(*                                                                        *)
(**************************************************************************)

(* $Id: dalton_tree.ml,v 1.2 2003/06/26 13:32:47 simonet Exp $ *)



type 'a t =
    Empty
  | Element of 'a * 'a t
  | Union of 'a t * 'a t



let empty =
  Empty

let add x t =
  Element (x, t)

let union t1 t2 =
  match t1, t2 with
    Empty, t | t, Empty -> t
  | _ -> Union (t1, t2)

let rec is_empty = function
    Empty -> true
  | Element _ -> false
  | Union (t1, t2) -> is_empty t1 && is_empty t2

let rec cardinal = function
    Empty -> 0
  | Element (_, t) -> 1 + cardinal t
  | Union (t1, t2) -> cardinal t1 + cardinal t2

let rec choose = function
    Empty -> raise Not_found
  | Element (x, _) -> x
  | Union (t1, t2) ->
      try choose t1
      with Not_found -> choose t2

let to_list t =
  let rec to_list_rec accu = function
      Empty -> accu
    | Element (x, t) -> to_list_rec (x :: accu) t
    | Union (t1, t2) -> to_list_rec (to_list_rec accu t1) t2
  in
  to_list_rec [] t

let rec iter f = function
    Empty -> ()
  | Element (x, t) -> f x; iter f t
  | Union (t1, t2) -> iter f t1; iter f t2

let map f t =
  let rec map_rec accu = function
      Empty -> accu
    | Element (x, t) -> map_rec (Element (f x, accu)) t
    | Union (t1, t2) -> map_rec (map_rec accu t1) t2
  in
  map_rec Empty t

let rec fold f t accu =
  match t with
    Empty -> accu
  | Element (x, t') -> fold f t' (f x accu)
  | Union (t1, t2) ->
      fold f t1 (fold f t2 accu)

let rec exists f = function
    Empty -> false
  | Element (x, t) -> f x || exists f t
  | Union (t1, t2) -> exists f t1 || exists f t2

let filter f t =
  let rec filter_rec accu = function
      Empty -> accu
    | Element (x, t) ->
	if f x then filter_rec (Element (x, accu)) t
	else filter_rec accu t
    | Union (t1, t2) ->
	filter_rec (filter_rec accu t1) t2
  in
  filter_rec Empty t

let filter_map f t =
  let rec filter_map_rec accu = function
      Empty -> accu
    | Element (x, t) ->
	begin match f x with
	  None -> filter_map_rec accu t
	| Some x' -> filter_map_rec (Element (x', accu)) t
	end
    | Union (t1, t2) ->
	filter_map_rec (filter_map_rec accu t1) t2
  in
  filter_map_rec Empty t


(* TEMPORARY *)
let sort cmp t =
  t

(* TEMPORARY *)
let rec compare cmp t1 t2 =
  match t1, t2 with
    Empty, Empty -> 0
  | Empty, (Element _ | Union _) | Element _, Union _ -> -1
  | (Element _ | Union _), Empty | Union _, Element _ -> 1
  | Element (x1, t1'), Element (x2, t2') ->
      let c = cmp x1 x2 in
      if c <> 0 then c else compare cmp t1' t2'
  | Union (t1', t1''), Union (t2', t2'') ->
      let c = compare cmp t1' t2' in
      if c <> 0 then c else compare cmp t1'' t2''


(***************************************************************************)
(** {2 Trees with markable elements} *)

module type MARKABLE = sig

  type t

  val mark: t -> bool
  val unmark: t -> unit

end



module Marked (X: MARKABLE) = struct

  let semaphore = 
    ref false

  let enter () =
    if !semaphore then false else begin
      semaphore := true;
      true
    end

  let leave () =
    semaphore := false;
    true


  let compact t =

    assert (enter ());

    let rec compact_rec accu = function
	Empty -> accu
      | Element (x, t) ->
	  if X.mark x then begin
	    let result = compact_rec (Element (x, accu)) t in
	    X.unmark x;
	    result
	  end
	  else compact_rec accu t
      | Union (t1, t2) ->
	  compact_rec (compact_rec accu t1) t2
    in

    let result = compact_rec Empty t in
    assert (leave ());
    result



  let compact_except x t =
    ignore (X.mark x);
    let result = compact t in
    X.unmark x;
    result


  let iter_diff f t1 t2 =

    assert (enter ());

    iter (function x -> ignore (X.mark x)) t2;
    iter (function x -> if X.mark x then f x) t1;
    iter X.unmark t1;
    iter X.unmark t2;

    assert (leave ())


  let iter f t =

    assert (enter ());

    let rec iter_rec = function
	Empty -> ()
      |	Element (x, t) ->
	  if X.mark x then begin
	    f x;
	    iter_rec t;
	    X.unmark x
	  end
	  else iter_rec t
      |	Union (t1, t2) ->
	  iter_rec t1;
	  iter_rec t2
    in

    iter_rec t;

    assert (leave ())



  let filter f t =


    assert (enter ());

    let rec filter_rec accu = function
	Empty -> accu
      |	Element (x, t) ->
	  if X.mark x then begin
	    let result = 
	      filter_rec (if f x then Element (x, accu) else accu) t
	    in
	    X.unmark x;
	    result
	  end
	  else filter_rec accu t
      |	Union (t1, t2) ->
	  filter_rec (filter_rec accu t1) t2
    in

    let result = filter_rec Empty t in
    assert (leave ());
    result

    

end



type 'a stest =
    SEmpty
  | SSingleton of 'a
  | SOther

let rec stest = function
    Empty -> SEmpty
  | Element (x, t) ->
      if is_empty t then SSingleton x else SOther
  | Union (t1, t2) ->
      match stest t1 with
	SEmpty -> stest t2
      |	(SSingleton _) as s -> if is_empty t2 then s else SOther
      |	SOther -> SOther
