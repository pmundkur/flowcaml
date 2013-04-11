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

(* $Id: avl_tarjan.mli,v 1.3 2003/06/26 13:32:45 simonet Exp $ *)

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



module Make (X : GRAPH) : sig

  (** [fold empty add_class singleton add g] runs the Tarjan's algorithm
      on the graph [g].  It returns a partition of the set of the nodes 
      of the graph (i.e. a set of set of nodes), which is computed as 
      follows:
      - [empty] is the empty partition,
      - [add_class c p] adds the class [c] to the partition [p],
      - [singleton nd] returns the class with one element [nd],
      - [add nd c] add the node [nd] to the class [c].
   *)
  val fold:
      'a -> (X.node -> 'a -> 'a) -> 'b -> ('a -> 'b -> 'b)
	-> X.graph -> 'b

  (** [list g] computes the SCCs of a graph.  The result is a list of list
      of nodes: each list gives the nodes of one of the SCCs.
   *)
  val list: X.graph -> X.node list list

  (** [unify unifier graph] computes the SCCs of a graph.  For every SCC,
      it chooses a particular node [nd], and for every other node [nd'] of
      the SCC, [unifier nd nd'] is called.
   *)
  val unify: (X.node -> X.node -> unit) -> X.graph -> unit

end
