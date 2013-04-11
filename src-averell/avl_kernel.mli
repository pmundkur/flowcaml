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

(* $Id: avl_kernel.mli,v 1.4 2003/06/26 13:32:45 simonet Exp $ *)

(** Transitive reduction of a directed graph.

    This module allows computing the transitive reduction of a directed 
    graph.  Given a acyclic graph {i G = (X, E)}, the transitive reduction
    of {i G} is the smallest graph {i G' = (X, E')} such that the transitive
    closure of {i G'} is {i G}.

    Reference:
    A.V. Aho, M.R. Garey and J.D. Ullman (HP).
    {i The Transitive Reduction of a Directed Graph}.
    SIAM Journal on Computing, 1(2), pp. 131-137, June 1972.
 *)



(***************************************************************************)
(** {2 Case of acyclic graphs} *)


(** This first implementation handles only acyclic graphs.
 *)

(** For this implementation, the client must provide an implementation of
    graph which fullfills the signature [GRAPH_acyclic ].
 *)
module type GRAPH_acyclic = sig

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



module Make_acyclic (X: GRAPH_acyclic) : sig

  exception Cyclic

  (** [fold g] computes the transitive reduction of the graph 
      [g].  The graph [g] is supposed to be acyclic, otherwise the
      exception [Cyclic] is raised.  The function returns the set of edges
      of the resulting graph.  This set is computed thanks to parameters
      [empty] and [add]:
      - [empty] is the empty initial set,
      - [add nd1 nd2 s] returns the set obtained by adding the edge
        [nd1 -> nd2] to the set [s].
   *)
  val fold:  'a -> (X.node -> X.node -> 'a -> 'a) -> X.graph -> 'a

  (** [list g] computes the transitive reduction of the graph
      [g].  The function returns the list of the edges of the
      resulting graph.  
   *) 
  val list: X.graph -> (X.node * X.node) list

end



(***************************************************************************)
(** {2 General case} *)

(** In the general case, nodes must provide an additional internal field
    of type ['a scc]. 
 *)
type 'a scc

(** [scc ()] returns a fresh value of type ['a scc]
 *)
val scc: unit -> 'a scc



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

  (** Every node must carry a transient field of type [node scc].
      The following functions allows reading and updating this field.
   *)
  val get_scc: node -> node scc
  val set_scc: node -> node scc -> unit

  (** Every node must carry a transient integer field.  No
      assumption is made about the initial content. The following
      functions allows reading and updating this field.
   *)
  val get: node -> int
  val set: node -> int -> unit

end



module Make (X: GRAPH) : sig

  (** [fold g] computes the transitive reduction of the graph [g].
   *)
  val fold: 'a -> (X.node -> X.node -> 'a -> 'a) -> X.graph -> 'a

  (** [list g] computes the transitive reduction of the graph
      [g].  The function returns the list of the edges of the
      resulting graph.  
   *) 
  val list: X.graph -> (X.node * X.node) list

end
