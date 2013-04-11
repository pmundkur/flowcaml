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

(* $Id: avl_topo.ml,v 1.2 2003/06/26 13:32:45 simonet Exp $ *)

(** Topological sort of a directed graph.

   Let {i G} be a graph whose nodes are $x_1, ..., x_n$.  Sorting $g$
   in the topological order consists in enumerating succesivelly the 
   nodes $x_s(1), ..., x_s(n)$ such that for all $i$ and $j$, if $g$
   has an edge $x_i -> x_j$ then $s^-1(i) < s^-1(j)$.
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



module Make (X: GRAPH) = struct

  (** [init graph] performs some preminilary computation on a graph required
      for topological sort.  Basically, it stores the number of predecessors
      of every node in its transient field; and returns a stack containing all
      the root of the graph (i.e. the nodes without any predecessor. *)
  let init graph =

    X.iter_nodes (function nd -> X.set nd 0) graph;

    (* The number of predecessors of every node is calculated into its
       transient field. *)
    X.iter_nodes (function nd ->
      X.iter_successors (function nd' ->
	X.set nd' (X.get nd' + 1)
      ) nd
    ) graph;

    (* A root is a node which does not have any predecessor.  The function 
       returns a stack containing the roots of the graph. *)
    let stack = Stack.create () in
    X.iter_nodes (function nd ->
      if X.get nd = 0 then Stack.push nd stack
    ) graph;
    stack



  (** [pop_safe s] returns [None] if the stack [s] is empty.  Otherwise,
      it removes the topmost element [x] in the stack [s] and returns
      [Some x]. *)
  let pop_safe stack =
    try
      Some (Stack.pop stack)
    with
      Stack.Empty -> None


  (** When applied on a graph containing a cycle, functions of this module
      raise the exception [Cyclic].
   *)
  exception Cyclic



  (** [fold f accu g] performs a topological sort of the graph g.  The 
      result is [f xn (f ... (f x2 (f x1 accu)))] where $x1, ..., xn$ is
      an acceptable ordering of the nodes. *)
  let fold f initial_accu graph =

    let stack = init graph in

    let rec fold_rec accu =

      match pop_safe stack with

	None -> accu

      | Some nd ->
	  X.iter_successors (function nd' ->
	    let i = X.get nd' in
	    X.set nd' (i - 1);
	    if i = 1 then Stack.push nd' stack
          ) nd;
	  fold_rec (f nd accu)

    in

    let result =  fold_rec initial_accu in

    X.iter_nodes (function nd ->
      if X.get nd <> 0 then raise Cyclic
    ) graph;

    result


  (** [iter f g] applies [f] on every node of the graph [g], in an 
      order compatible with topological sort. 
   *)
  let iter f graph =
    fold (fun nd _ -> f nd) () graph



  (** [list g] returns the list of nodes of [g] ordered topologically.
   *)
  let list graph =
    fold (fun nd accu -> nd :: accu) [] graph



  (** [stack g] returns a stack containing the nodes of [g] ordered 
      topologically.
   *)
  let stack graph =
    let s = Stack.create () in
    iter (function node -> Stack.push node s) graph;
    s



  (** [queue g] returns a queue containing the nodes of [g] ordered
      topologically.
   *)
  let queue graph =
    let q = Queue.create () in
    iter (function node -> Queue.add node q) graph;
    q

end
