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

type 'a t = 'a list

let empty = []

let add x t = x :: t

let union = List.rev_append

let is_empty = function
    [] -> true
  | _ -> false

let choose = function
    [] -> raise Not_found
  | x :: _ -> x

let to_list t = t

let iter = List.iter

let map = List.map

let fold f t accu = List.fold_left (fun a b -> f b a) accu t

let exists = List.exists

let filter = List.filter

let rec filter_map f = function
    [] -> []
  | x :: t -> 
      match f x with
	None -> filter_map f t
      |	Some x' -> x' :: filter_map f t

let sort = List.sort

let compare = Dalton_lib.Standard.compare_list

let classes = Dalton_lib.Standard.classes

module type MARKABLE = sig

  type t

  val mark: t -> bool
  val unmark: t -> unit

end

module Marked (X: MARKABLE) = struct

  type elt = X.t
  type t = elt list

  let semaphore =
    ref false

  let enter () =
    if !semaphore then false
    else begin
      semaphore := true;
      true
    end

  let leave () =
    if not (!semaphore) then false
    else begin
      semaphore := false;
      true
    end

  let iter f list =
    assert (enter ());
    let rec iter_rec = function
	[] -> ()
      | hd :: tl ->
	  if X.mark hd then begin
	    f hd;
	    iter_rec tl;
	    X.unmark hd
	  end
	  else iter_rec tl
    in
    iter_rec list;
    assert (leave ())
    


  let filter f list =
    assert (enter ());
    let rec filter_rec = function
	[] -> []
      | hd :: tl ->
	  if X.mark hd then begin
	    let tl' = filter_rec tl in
	    X.unmark hd;
	    if f hd then hd :: tl' else tl'
	  end
	  else filter_rec tl
    in
    let list' = filter_rec list in
    assert (leave ());
    list'



  let compact list =
    assert (enter ());
    let rec clean_rec = function
	[] -> []
      | (hd :: tl) as hdtl ->
	  if X.mark hd then begin
	    let tl' = clean_rec tl in
	    X.unmark hd;
	    if tl == tl' then hdtl else hd :: tl'
	  end
	  else clean_rec tl
    in
    let list' = clean_rec list in
    assert (leave ());
    list'

  let compact_except x list =
    assert (enter ());
    let rec clean_rec = function
	[] -> []
      | (hd :: tl) as hdtl ->
	  if X.mark hd then begin
	    let tl' = clean_rec tl in
	    X.unmark hd;
	    if tl == tl' then hdtl else hd :: tl'
	  end
	  else clean_rec tl
    in
    ignore (X.mark x);
    let list' = clean_rec list in
    X.unmark x;
    assert (leave ());
    list'

  (** [iter_except f list1 list2] applies [f] on every element of [list1] which
      is not in [list2].
   *)
  let iter_diff f list1 list2 =

    assert (enter ());
    List.iter (function elt -> ignore (X.mark elt)) list2;
    List.iter (function elt -> if X.mark elt then  f elt) list1;
    List.iter (function elt -> X.unmark elt) list2;
    assert (leave ())
end

type 'a stest =
    SEmpty
  | SSingleton of 'a
  | SOther

let stest = function
    [] -> SEmpty
  | [x] -> SSingleton x
  | _ :: _ :: _ -> SOther
