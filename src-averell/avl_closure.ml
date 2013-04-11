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

(* $Id: avl_closure.ml,v 1.4 2003/06/26 13:32:44 simonet Exp $ *)

(** Transitive closure of a directed graph.

    This module allows computing the transitive closure of a directed 
    graph.  Given a graph {i G = (X, E)}, the strict transitive closure
    of {i G} is the graph {i G' = (X, E')} such that there is an edge 
    from {i x} to {i y} in {i G'} if and only if there is a non-empty 
    path from {i x} to {i y} in {i G}.
 *)



(** The client must provide an implementation of graph which
    fullfills the signature [GRAPH].
 *)
module type GRAPH = sig

  (** The type of graphs. *)
  type graph

  (** The type of nodes. *)
  type node

  (** [iter_nodes f g] applies [f] on every nodes of the graph [g].  The 
      order in which nodes are considered does not matter.  However, each
      node must be considered exactly once. 
   *)
  val iter_nodes: (node -> unit) -> graph -> unit

  (** [iter_successors f nd] applies [f] on every successors of the node
      [nd] in its graph.  The order in which successors are considere does
      not matter.  Multiple occurences of the same successor are allowed.
   *)
  val iter_successors: (node -> unit) -> node -> unit

  (** Every node must carry a transient integer field.  No
      assumption is made about the initial content. The following
      functions allows reading and updating this field.
   *)
  val get: node -> int
  val set: node -> int -> unit

end




module Make (X: GRAPH) = struct

  (** [fold empty add g] computes the strict transitive closure
      of the graph [g].  The function returns the set of edges of the 
      resulting graph.  This set is built thanks to parameters [empty]
      and [add]:
      - [empty] is the empty initial set,
      - [add nd1 nd2 s] returns the set obtained by adding the edge
        [nd1 -> nd2] to the set [s].
   *)
  let fold empty add graph =

    (* Reset transient fields. *)
    X.iter_nodes (function node -> X.set node 0) graph;

    let counter = ref 0 in
    let result = ref empty in

    X.iter_nodes (function node ->

      incr counter;
      X.set node !counter;

      let rec loop node' =
	if X.get node' <> !counter then begin
	  X.set node' !counter;
	  result := add node node' !result;
	  X.iter_successors loop node'
	end
      in

      X.iter_successors loop node;

    ) graph;

    !result

  (** [list g] computes the strict transitive closure of the graph
      [g].  The function returns the list of the edges of the
      resulting graph.  
   *) 
  let list graph = 
    fold [] (fun nd1 nd2 tl -> (nd1, nd2) :: tl) graph

end
