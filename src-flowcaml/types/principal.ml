(**************************************************************************)
(*                                                                        *)
(*                               Flow Caml                                *)
(*                                                                        *)
(*          Vincent Simonet, Projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*  Copyright 2002, 2003 Institut National de Recherche en Informatique   *)
(*  et en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.                  *)
(*                                                                        *)
(*  Author contact: Vincent.Simonet@inria.fr                              *)
(*  Software page: http://cristal.inria.fr/~simonet/soft/flowcaml/        *)
(*                                                                        *)
(**************************************************************************)

(* $Id: principal.ml,v 1.4 2003/10/01 13:28:21 simonet Exp $ *)
(* Principal: the lattice of principals *)

open Datastruct



type node =
    { mutable succ: node StringMap.t;
      mutable succ_kernel: node StringMap.t;
      mutable counter: int;
      mutable scc: (string * node) Avl_kernel.scc
    } 

type lattice =
    node StringHashtbl.t



(*************************************************************************)
(** {2 Graph operations on lattices} *)

module G = struct

  type graph = lattice
  type foo = node
  type node = string * foo

  let iter_nodes f graph =
    StringHashtbl.iter (fun name node -> f (name, node)) graph
  let iter_successors f (_, node) =
    StringMap.iter (fun name' node' -> f (name', node')) node.succ

  let get (_, node) = 
    node.counter
  let set (_, node) i =
    node.counter <- i
  let get_scc (_, node) = 
    node.scc
  let set_scc (_, node) i =
    node.scc <- i

end

module Closure = Avl_closure.Make (G)
module Kernel = Avl_kernel.Make (G)



(*************************************************************************)
(** {2 Manipulating lattices} *)

let create () =
  StringHashtbl.create 7

let dumb_scc = Avl_kernel.scc ()



(** [find lattice principal] returns the node representing the principal
    [principal] in the lattice [lattice].  If [principal] is not represented
    in [lattice], the exception [Not_found] is raised.
 *)
let find lattice principal =
  StringHashtbl.find lattice principal



(** [get lattice principal] returns the node representing the principal
    [principal] in the lattice [lattice].  If [principal] is not represented
    in [lattice], a new node is created, inserted in [lattice] and returned.
 *)
let get lattice principal =
  try
    find lattice principal
  with
    Not_found ->
      let node = 
	{ succ = StringMap.empty;
	  succ_kernel = StringMap.empty;
	  counter = 0;
	  scc = dumb_scc
	} 
      in
      StringHashtbl.add lattice principal node;
      node



(*************************************************************************)
(** {2 Translating flows declarations into lattices} *)

(** [flow lattice principal1 principal2] registers the flow from 
    [principal1] to [principal2] in the lattice [lattice].  It does not
    performs the transitive closure and reduction of the underlying graph.
 *)
let add_flow lattice principal1 principal2 =
  let node1 = get lattice principal1 in
  if not (StringMap.mem principal2 node1.succ) then begin
    let node2 = get lattice principal2 in
    node1.succ <- StringMap.add principal2 node2 node1.succ
  end



(** [do_closure lattice] performs the transitive closure of the lattice
    [lattice].
 *)
let do_closure lattice =
  let edges = Closure.list lattice in
  StringHashtbl.iter (fun _ node -> node.succ <- StringMap.empty) lattice;
  List.iter (function (principal1, node1), (principal2, node2) ->
    node1.succ <- StringMap.add principal2 node2 node1.succ
  ) edges




(** [do_kernel lattice] computes the transitive reduction of the lattice
    [lattice].
 *)
let do_kernel lattice =
  StringHashtbl.iter (fun _ node -> node.succ_kernel <- StringMap.empty) lattice;
  Kernel.fold () (fun (principal1, node1) (principal2, node2) () ->
    node1.succ_kernel <- StringMap.add principal2 node2 node1.succ_kernel
  ) lattice



(** [translate_into lattice plat] add every flow listed in [plat] in the
    lattice [lattice].  At the end of insertion, [lattice] is transitively
    closed.
 *)
let translate_into lattice plat =

  List.iter (function principals_from, principals_to ->

    List.iter (function principal_from ->
      List.iter (function principal_to ->
	add_flow lattice principal_from principal_to
      ) principals_to
    ) principals_from

  ) plat;

  do_closure lattice



(** [translate plat] creates a new lattice with all flows listed in [plat].
 *)
let translate plat =
  let lattice = create () in
  translate_into lattice plat;
  lattice



(** [merge_into lattice1 lattice2] inserts all the inequalities of [lattice1]
    in [lattice2].
 *)
let merge_into lattice1 lattice2 =

  StringHashtbl.iter (fun principal node2 ->
    let node1 = get lattice1 principal in
    StringMap.iter (fun principal' _ ->
      if not (StringMap.mem principal' node1.succ) then begin
	let node1' = get lattice1 principal' in
	node1.succ <- StringMap.add principal' node1' node1.succ
      end
    ) node2.succ
  ) lattice2



(*************************************************************************)
(** {2 Inclusion test} *)

exception Included of string * string

let included lattice1 lattice2 =

  try

    StringHashtbl.iter (fun name desc1 ->

      let desc2 = get lattice2 name in

      StringMap.iter (fun name' _ ->
	if not (StringMap.mem name' desc2.succ) then 
	  raise (Included (name, name'))
      ) desc1.succ

    ) lattice1;

    None

  with

    Included (name1, name2) -> Some (name1, name2)



(*************************************************************************)
(** {2 Testing inequalities} *)

let leq lattice principal1 principal2 =

  (principal1 = principal2)

    or

  try 
    let node1 = find lattice principal1 in
    StringMap.mem principal2 node1.succ

  with
    Not_found -> false



(*************************************************************************)
(** {2 Output functions} *)


let fprint ppf lattice =
  do_kernel lattice;
  Format.fprintf ppf "@[<v>";
  StringHashtbl.iter (fun name desc ->
    StringMap.iter (fun name' _ ->
      Format.fprintf ppf "@[!%s < !%s@]@ " name name'
    ) desc.succ_kernel
  ) lattice;
  Format.fprintf ppf "@]"



module Draw = Avl_draw.MakeGraphics (struct

  open Avl_draw

  let dark_blue = Avl_graphics.rgb 0 0 128
  let dark_red = Avl_graphics.rgb 128 0 0

  let flow_node name =
    { default_node with
      nd_shape = `Box;
      nd_border = `Solid (dark_blue, 0);
      nd_label = `Text { default_label with 
			 tl_text = name;
			 tl_color = dark_red }
    }

  let flow_edge =
    { default_edge with
      ed_linestyle = `Solid (dark_red, 0);
      ed_tailarrow = `Filled (10, 0.5 *. atan 1.0, dark_red)
    }



  type graph = lattice
  type foo = node
  type node = string * foo
  type edge = node * node

  let iter_nodes f graph =
    StringHashtbl.iter (fun name desc -> f (name, desc)) graph
  let iter_successors f (_, desc) =
    StringMap.iter (fun name' desc' -> f (name', desc')) desc.succ_kernel

  let get (_, desc) = 
    desc.counter
  let set (_, desc) i =
    desc.counter <- i

  let iter_edges f graph =
    iter_nodes (function node ->
      iter_successors (function node' -> f (node, node')) node
    ) graph

  let node_hash (name, _) = 
    Hashtbl.hash name
  let node_equal (name1, desc1) (name2, desc2) =
    name1 = name2
  let node_attributes (name, _) =
    flow_node ("!" ^ name)

  let edge_hash ((name1, _), (name2, _)) =
    Hashtbl.hash (name1, name2)
  let edge_equal (node1, node2) (node1', node2') =
    node_equal node1 node1' && node_equal node2 node2'
  let edge_head (node, _) =
    node
  let edge_tail (_, node) =
    node
  let edge_attributes (node1, node2) =
    flow_edge

end)

let draw lattice x y =
  let w, h = Avl_graphics.text_size "X" in
  do_kernel lattice;
  Draw.draw_graph () (`Dot [`Nodesep w; `Ranksep h]) x y lattice
