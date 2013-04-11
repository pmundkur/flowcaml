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

(* $Id: avl_kernel.ml,v 1.4 2003/06/26 13:32:45 simonet Exp $ *)

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



module Make_acyclic (X: GRAPH_acyclic) = struct

  module Topo = Avl_topo.Make (X)

  exception Cyclic

  (** [fold g] computes the transitive reduction of the graph 
      [g].  The graph [g] is supposed to be acyclic, otherwise the
      exception [Cyclic] is raised.
   *)
  let fold empty add graph =

    let sorted_list = 
      try Topo.list graph
      with Topo.Cyclic -> raise Cyclic
    in

    assert (List.for_all (function node -> X.get node = 0) sorted_list);

    let rec mark_successors flag node =
      X.iter_successors (function node' ->
	if X.get node' <> flag then begin
	  X.set node' flag;
	  mark_successors flag node'
	end
      ) node
    in

    let result = ref empty in

    List.iter (function node ->
      
      X.iter_successors (mark_successors 1) node;
      
      let s = Stack.create () in
      X.iter_successors (function node' -> Stack.push node' s) node;

      Stack.iter (function node' ->
	if X.get node' = 0 then result := add node node' !result
      ) s;

      X.iter_successors (mark_successors 0) node

    ) sorted_list;

    !result



  (** [list g] computes the transitive reduction of the graph
      [g].  The function returns the list of the edges of the
      resulting graph.  
   *) 
  let list graph = 
    fold [] (fun nd1 nd2 tl -> (nd1, nd2) :: tl) graph

end



(***************************************************************************)
(** {2 General case} *)


(** In the general case, nodes must provide an additional internal field
    of type ['a scc]. 
 *)
type 'a scc =
    { nodes: 'a list;
      mutable counter: int
    } 

(** [scc ()] returns a fresh value of type ['a scc]
 *)
let scc () =
  { nodes = [];
    counter = 0
  } 



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




module Make (X: GRAPH) = struct

  module SCC = struct

    type node = X.node scc
    type graph = node list

    let iter_nodes f graph =
      List.iter f graph

    let iter_successors f scc =
      List.iter (function node ->
	X.iter_successors (function node' -> 
	  let scc' = X.get_scc node' in
	  if scc' != scc then f scc'
	) node
      ) scc.nodes

    let get scc =
      scc.counter

    let set scc i =
      scc.counter <- i

  end

  module T = Avl_tarjan.Make (struct include X let same_node = (=) end)
      (* TEMPORARY *)

  module A = Make_acyclic (SCC)


  (** [fold g] computes the transitive reduction of the graph [g].
   *)
  let fold empty add g =

    (* [scc_graph] is the acyclic graph underlying [g]. *)
    let scc_graph =
      T.fold
	[] (fun node list -> node :: list)
	[] (fun scc partition -> { nodes = scc; counter = 0 } :: partition)
	g
    in

    (* Each node is registered into its scc. *)
    List.iter (function scc ->
      List.iter (function node -> X.set_scc node scc) scc.nodes
    ) scc_graph;

    (* The graph [scc_graph] is reduced. *)
    let scc_add scc1 scc2 s =
      match scc1.nodes, scc2.nodes with
	[], _ | _, [] -> assert false
      | nd1 :: _, nd2 :: _ -> add nd1 nd2 s
    in
    let result = ref (A.fold empty scc_add scc_graph) in

    (* Transient fields of nodes are cleared.  Internal egdes are inserted
       into each SCC. *)
    let dumb = scc () in

    List.iter (function scc ->
      begin match scc.nodes with
	[] -> assert false
      | _ :: [] -> ()
      |	node :: tl ->
	  let node' =
	    List.fold_left (fun node' node ->
	      result := add node' node !result;
	      node
            ) node tl
	  in
	  result := add node' node !result
      end;
      List.iter (function node -> X.set_scc node dumb) scc.nodes

    ) scc_graph;

    !result



  (** [list g] computes the transitive reduction of the graph
      [g].  The function returns the list of the edges of the
      resulting graph.  
   *) 
  let list graph = 
    fold [] (fun nd1 nd2 tl -> (nd1, nd2) :: tl) graph

end
