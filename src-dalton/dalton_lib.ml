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

(* $Id: dalton_lib.ml,v 1.2 2003/06/26 13:32:47 simonet Exp $ *)

(** Library

    This module provide some library functions used by Dalton.
 *)



(***************************************************************************)
(** {2 Standard library add-ons} *)

module Standard = struct

  (** [filter_map] expects a ``constructive predicate'' [p] and a list
      [l]. A ``constructive predicate'' is a function which, given an
      element [x], returns either another [x'], or nothing. [filter_map p
      l] returns the list obtained by applying [p] to each element of
      [l], and gathering any elements returned by [p]. [filter_map] is
      thus a combination of [filter] and [map]. 
   *)
  let filter_map f list =
    let rec map_filter_rec = function
	[] -> []
      | t :: q ->
	  match f t with
	    None -> map_filter_rec q
	  | Some x -> x :: map_filter_rec q
    in
    map_filter_rec list


  (** [classes cmp e] returns the list of lists of equals elements of [e],
      according to the total order [cmp].
   *)
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



  (** Given a function [compare_elt] ordering values of type ['a],
      [compare_list compare_elt] returns a function ordering list of
      type ['a list].
   *)
  let compare_list compare_elt =
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

end



(***************************************************************************)
(** {2 Union-find} *)

(** The module [Proxy] defines data structures which support a basic
    union/find algorithm. These will be useful in every implementation
    of unification. 
 *)
module Proxy = struct

  (** A [proxy] is a cell which represents a point in some graph. It has
      physical identity. Its contents simply consist of a mutable [link],
      which leads either directly to a [descriptor] of unknown type ['a],
      or to another [proxy], in which case the first proxy is to be
      viewed as an alias for the second one.
   *)
  type 'a t = {
      mutable link : 'a link
    } 

  and 'a link = 
      NoLink of 'a
    | LinkTo of 'a t



  (** [create desc] returns a fresh proxy for the specified descriptor. 
   *)
  let create desc =
    { link = NoLink desc }



  (** [repr proxy] returns a proxy's representative, that is, a canonical
      proxy for which [proxy] is an alias. It internally performs path
      compression. 
   *)
  let rec repr proxy =
    match proxy.link with
      NoLink _ -> proxy
    | LinkTo proxy' ->
	let proxy'' = repr proxy' in
	if proxy'' != proxy' then
	  proxy.link <- proxy'.link;
	proxy''



  (** [desc proxy] returns the descriptor associated with the proxy [proxy]. 
   *)
  let rec desc proxy =
    match proxy.link with
      NoLink desc -> desc
    | LinkTo proxy -> desc (repr proxy)



  (** [linksto proxy1 proxy2] makes [proxy1] an alias for [proxy2]. 
      [proxy1]'s descriptor becomes no longer reachable through [proxy1].
   *)
  let linksto proxy1 proxy2 =
    let proxy1 = repr proxy1 
    and proxy2 = repr proxy2
    in
    if proxy1 != proxy2 then
      proxy1.link <- LinkTo proxy2


  (** [principal proxy] returns [true] for exactly one member of the
      equivalence class. 
   *)
  let principal = function
    | { link = NoLink _ } ->
	true
    | { link = LinkTo _ } ->
	false

end



(***************************************************************************)
(** {2 Marks} *)

module Mark = struct

  type t = unit ref

  let create () =
    ref ()

  let same =
    (==)

  let none =
    create ()

end



(***************************************************************************)

module type M = sig

  type t

  val mark: t -> bool
  val unmark: t -> unit

end



module MList_onepass (X: M) = struct

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



  let clean list =
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



  let union_except list1 list2 except =

    assert (enter ());
    List.iter (function elt -> ignore (X.mark elt)) except;

    let rec union_except_rec accu = function
	[] -> accu
      | hd :: tl ->
	  if X.mark hd then begin
	    let result = union_except_rec accu tl in
	    X.unmark hd;
	    hd :: result
	  end
	  else union_except_rec accu tl
    in

    let result = union_except_rec (union_except_rec [] list2) list1 in

    List.iter (function elt -> X.unmark elt) except;
    assert (leave ());

    result



  let union list1 list2 =

    assert (enter ());

    let rec union_rec accu = function
	[] -> accu
      | hd :: tl ->
	  if X.mark hd then begin
	    let result = union_rec accu tl in
	    X.unmark hd;
	    hd :: result
	  end
	  else union_rec accu tl
    in

    let result = union_rec list1 list2 in
    assert (leave ());
    result



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


module MList_tailrec (X: M) = struct

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

  let unmark list =
    List.iter X.unmark list

  let iter f list =
    assert (enter ());
    List.iter (function elt -> if X.mark elt then f elt) list;
    unmark list;
    assert (leave ())
    


  let filter f list =
    assert (enter ());
    let result = List.filter (function elt -> X.mark elt && f elt) list in
    unmark list;
    assert (leave ());
    result



  let clean list =
    assert (enter ());
    let result = List.filter X.mark list in
    unmark result;
    assert (leave ());
    result



  let union_except list1 list2 except =

    assert (enter ());
    List.iter (function elt -> ignore (X.mark elt)) except;

    let rec union_except_rec accu = function
	[] -> accu
      | hd :: tl ->
	  if X.mark hd then union_except_rec (hd :: accu) tl
	  else union_except_rec accu tl
    in

    let result = union_except_rec (union_except_rec [] list2) list1 in

    unmark except;
    unmark result;
    assert (leave ());

    result



  let union list1 list2 =

    assert (enter ());

    let rec union_rec accu = function
	[] -> accu
      | hd :: tl ->
	  if X.mark hd then union_rec (hd :: accu) tl
	  else union_rec accu tl
    in

    let result = union_rec list1 list2 in
    unmark result;
    assert (leave ());
    result



  (** [iter_except f list1 list2] applies [f] on every element of [list1] which
      is not in [list2].
   *)
  let iter_diff f list1 list2 =

    assert (enter ());
    List.iter (function elt -> ignore (X.mark elt)) list2;
    iter f list1;
    unmark list2;
    assert (leave ())

end



module MList = MList_onepass (* TEMPORARY C le plus rapide *)



(***************************************************************************)
(** {2 Lists of markable elements} *)

module type MARKABLE = sig

  type t

  val get: t -> Mark.t
  val set: t -> Mark.t -> unit

end



module Marked_list (X: MARKABLE) = struct

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
    let mark = Mark.create () in
    List.iter (function x ->
      if not (Mark.same mark (X.get x)) then begin
	X.set x mark;
	f x
      end
    ) list;
    assert (leave ());
    ()

  let exists f list =
    assert (enter ());
    let mark = Mark.create () in
    let result = 
      List.exists (function x ->
	if Mark.same mark (X.get x) then false else begin
	  X.set x mark;
	  f x
	end
      ) list
    in
    assert (leave ());
    result

  let fold f list accu =
    assert (enter ());
    let mark = Mark.create () in
    let result =
      List.fold_left (fun accu x ->
	if Mark.same mark (X.get x) then accu else begin
	  X.set x mark;
	  f x accu
	end
      ) accu list
    in
    assert (leave ());
    result

  let filter f list =
    assert (enter ());
    let mark = Mark.create () in
    let result =
      List.filter (function x ->
	if Mark.same mark (X.get x) then false
	else begin
	  X.set x mark;
	  f x
	end
      ) list
    in
    assert (leave ());
    result

  let clean list =
    assert (enter ());
    let mark = Mark.create () in
    let result =
      List.filter (function x -> 
	if Mark.same mark (X.get x) then false
	else begin
	  X.set x mark;
	  true
	end
      ) list
    in
    assert (leave ());
    result

  let is_clean list =
    assert (enter ());
    let mark = Mark.create () in
    let result =
      List.for_all (function x ->
	if Mark.same mark (X.get x) then false
	else begin
	  X.set x mark;
	  true
	end
      ) list
    in
    assert (leave ());
    result

  let union_except list1 list2 except =

    assert (enter ());

    let mark = Mark.create () in

    List.iter (function x -> X.set x mark) except;

    let rec union_except_rec1 accu = function
	[] -> accu
      | x :: tail ->
	  if Mark.same mark (X.get x) 
	  then union_except_rec1 accu tail
	  else begin
	    X.set x mark;
	    union_except_rec1 (x :: accu) tail
	  end
    in

    let rec union_except_rec2 accu = function
	[] -> union_except_rec1 accu list1
      | x :: tail ->
	  if Mark.same mark (X.get x) 
	  then union_except_rec2 accu tail
	  else begin
	    X.set x mark;
	    union_except_rec2 (x :: accu) tail
	  end
    in

    let result = union_except_rec2 [] list2 in

    assert (leave ());
    result



  let union list1 list2 =

    assert (enter ());
    let mark = Mark.create () in

    let rec union_rec accu = function

	[] -> accu

      | hd :: tl ->
	  if Mark.same mark (X.get hd) then union_rec accu tl
	  else begin
	    X.set hd mark;
	    union_rec (hd :: accu) tl
	  end

    in

    let result = union_rec list1 list2 in

    assert (leave ());
    result

  (** [iter_except f list1 list2] applies [f] on every element of [list1] which
      is not in [list2].
   *)
  let iter_diff f list1 list2 =
    let mark = Mark.create () in
    List.iter (function x -> X.set x mark) list2;
    List.iter (function x ->
      if not (Mark.same (X.get x) mark) then f x
    ) list1

end



(***************************************************************************)
(** {2 Hash-consing} *)

module type HASHABLE = sig

  (* The type of elements on which hash-consing operates. *)

  type t

  (* [t] must be a "hashed type", i.e. the client must provide two
     functions [equal] and [hash] such that if [equal x y] is [true]
     then [hash x] and [hash y] are equal.  However if there is no [y]
     such that [equal x y] holds then [hash x] may raise the exception
     [No_hash]. *)

  exception No_hash

  val equal: t -> t -> bool
  val hash: t -> int

  (* The [unify] function will be applied on pairs of equal elements.  
     This function must not change the result of [equal] and [hash] functions
     on its arguments. *)

  val unify: t -> t -> unit

end



module Hash_consing (X : HASHABLE) = struct

  module H = Hashtbl.Make (X)

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

  (* [f iter i] performs the hash-consing.  [iter f] must apply [f]
     succesively on every elements.  Be careful: [unify] will be
     called during the walk.  
   *)

  let f iter n =

    let hashtbl =  H.create n in

    assert (enter ());

    iter (fun x ->

      try 

	if H.mem hashtbl x then begin 

	  let x' = H.find hashtbl x in
	  X.unify x' x

	end

	else H.add hashtbl x x

      with X.No_hash -> ()

    );

    assert (leave ())

end
