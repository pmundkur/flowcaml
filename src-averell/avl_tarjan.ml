(**************************************************************************)
(*                                                                        *)
(*                                 Averell                                *)
(*                                                                        *)
(*          Vincent Simonet, Projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*  Copyright 2002, 2003 Institut National de Recherche en Informatique   *)
(*  et en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with the   *)
(*  special exception on linking described in file LICENSE.               *)
(*                                                                        *)
(*  Author contact: Vincent.Simonet@inria.fr                              *)
(*  Software page: http://cristal.inria.fr/~simonet/soft/                 *)
(*                                                                        *)
(**************************************************************************)

(* $Id: avl_tarjan.ml,v 1.3 2003/06/26 13:32:45 simonet Exp $ *)

(** Tarjan's algorithm: calculating SCC of a graph in linear time.

    This module provides an implementation of Tarjan's algorithm.  This
    algorithm computes the strong connex composants of a directed graph
    (SCC).  A SCC of a graph $G = (X, E)$ is a subset of $X$ such that for
    every pair of nodes $x_1$ and $x_2$ in $X$ there exists a path from
    the former to the latter in $G$.  The SCCs of $G$ form a partition of 
    $X$.  

    The Tarjan's algorithm has a time complexity in $O(n)$ (where $n$ is the
    number of nodes of the input graph).

    Functions provided by this module are not reentering thread safe as long 
    as at most one thread operates on the same graph at the same time.
 *)



(** The client must provide an implementation of graphs which fullfills the
    signature [GRAPH]. 
 *)
module type GRAPH = sig

  (** The type of graphs. *)
  type graph

  (** The type of nodes. *)
  type node

  (** [iter_nodes f g] applies [f] on every nodes of the graph [g].  The 
      order in which nodes are considered does not matter and may change
      between different application of the function on the same graph.  
      However, each node must be considered exactly once.
   *)
  val iter_nodes: (node -> unit) -> graph -> unit

  (** [iter_successors f nd] applies [f] on every successors of the
      node [nd] in its graph.  The order in which successors are
      considere does not matter.  The graph is not required to be
      simple (i.e. [iter_successors f nd] may apply [f] an arbitrary
      number of time the function [f] on each of [nd]'s successors, as
      long as this number is fixed.
   *)
  val iter_successors: (node -> unit) -> node -> unit

  (** Every node must carry a transient integer field.  The following
      functions allows reading and updating this field.  No assumption
      is made by the module on the initial content of this field at
      each function call.  However, the client cannot make any
      assumption on the final content too.
   *) 
  val get: node -> int
  val set: node -> int -> unit

end



(* The internal integer fields of nodes are used to store three informations:
   - a boolean (bit 0)
   - the number given to the node (bits 1 to 15)
   - the attache number of the node (bits 16 to 31)
 *)

let shift_attache i = i lsl 16
let unshift_attache i = i lsr 16

let get_tremaux i =
  i land 0b1111111111111111

let get_attache i =
  (unshift_attache i) land 0b1111111111111111


module Make (X : GRAPH) = struct

  (** [fold empty add_class singleton add g] runs the Tarjan's algorithm
      on the graph [g].  It returns a partition of the set of the nodes 
      of the graph (i.e. a set of set of nodes), which is computed as follows:
      - [empty] is the empty partition,
      - [add_class c p] adds the class [c] to the partition [p],
      - [singleton nd] returns the class with one element [nd],
      - [add nd c] add the node [nd] to the class [c].
   *)
  let fold empty1 add1 empty2 add2 graph =

    (* We ensure that number and attache fields of each node
       are equal to 0. *)

    X.iter_nodes (function nd -> X.set nd (-1)) graph;

    let stack = Stack.create () in
    let counter = ref 0 in
    let result = ref empty2 in

    let rec tremaux_walk nd =

      if X.get nd = -1 then begin

	Stack.push nd stack;

	let tremaux = incr counter; !counter in
	X.set nd tremaux;

	let attache = ref tremaux in

	X.iter_successors (function nd' ->
	  tremaux_walk nd';
	  let nd'i = X.get nd' in
	  if nd'i <> 0 then begin
	    let tremaux' = get_tremaux nd'i in
	    let new_attache = 
	      if tremaux' > tremaux then get_attache nd'i else tremaux'
	    in
	    if new_attache < !attache then attache := new_attache	  
	  end
        ) nd;

	let ndi = tremaux lor (shift_attache !attache) in
	X.set nd ndi;

	if tremaux = !attache then begin

	  let rec unroll accu =
	    let nd' = Stack.pop stack in
	    if X.get nd' = ndi then (X.set nd' 0; add1 nd' accu)
	    else (X.set nd' 0; unroll (add1 nd' accu))
	  in

	  result := add2 (unroll empty1) !result

	end;

      end

    in

    X.iter_nodes tremaux_walk graph;
    assert (Stack.is_empty stack);

    !result



  (** [list g] computes the SCCs of a graph.  The result is a list of list
      of nodes: each list gives the nodes of one of the SCCs.
   *)
  let list graph =

    fold
      [] (fun c p -> c :: p)
      [] (fun nd c -> nd :: c)
      graph



  (** [unify unifier graph] computes the SCCs of a graph.  For every SCC,
      it chooses a particular node [nd], and for every other node [nd'] of
      the SCC, [unifier nd nd'] is called.
   *)
  let unify unifier graph =

    fold
      None (fun nd' nd_opt -> 
	match nd_opt with
	  None -> Some nd'
	| Some nd -> unifier nd nd'; nd_opt
      )
      () (fun _ _ -> ())
      graph

end
