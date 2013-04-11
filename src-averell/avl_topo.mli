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

(* $Id: avl_topo.mli,v 1.2 2003/06/26 13:32:46 simonet Exp $ *)

(** Topological sort of a graph.

   Let {i G} be a graph whose nodes are {i x{_1}, ..., x{_n}}.  Sorting
   {i G} in the topological order consists in enumerating succesivelly the 
   nodes {i x{_s(1)}, ..., x{_s(n)}} such that for all {i i} and {i j}, if
   {i G} has an edge {i x{_s(i)} -> x{_(j)}} then {i i < j}.
 *)



(** The client must provide an implementation of graph which fullfills the
    signature [GRAPH]. *)

module type GRAPH = sig

  (** The type of graphs. *)
  type graph

  (** The type of nodes. *)
  type node

  (** [iter_nodes f g] applies [f] on every nodes of the graph [g].  The 
      order in which nodes are considered does not matter.  However, each
      node must be considered exactly once. *)
  val iter_nodes: (node -> unit) -> graph -> unit

  (** [iter_successors f nd] applies [f] on every successors of the node
      [nd] in its graph.  The order in which successors are considere does
      not matter.  Multiple occurences of the same successor are allowed.
   *)
  val iter_successors: (node -> unit) -> node -> unit

  (** Every node must carry a transient integer field.  No assumption
      is made on the initial content of this field.  The following
      functions allows reading and updating this field.  *) 
  val get: node -> int 
  val set: node -> int -> unit

end



module Make (X: GRAPH) : sig

  (** When applied on a graph containing a cycle, functions of this module
      raise the exception [Cyclic].
   *)
  exception Cyclic



  (** [fold f accu g] performs a topological sort of the graph g.  The
      result is [f xn (f ... (f x2 (f x1 accu)))] where {i x{_1}, ...,
      x{_n}} is an acceptable ordering of the nodes.  *) val fold:
      (X.node -> 'a -> 'a) -> 'a -> X.graph -> 'a

  (** [iter f g] applies [f] on every node of the graph [g], in an 
      order compatible with topological sort. 
   *)
  val iter: (X.node -> unit) -> X.graph -> unit

  (** [list g] returns the list of nodes of [g] ordered topologically.
   *)
  val list: X.graph -> X.node list

  (** [stack g] returns a stack containing the nodes of [g] ordered 
      topologically.
   *)
  val stack: X.graph -> X.node Stack.t

  (** [queue g] returns a queue containing the nodes of [g] ordered
      topologically.
   *)
  val queue: X.graph -> X.node Queue.t

end
