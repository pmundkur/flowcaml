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

(* $Id: demo.ml,v 1.5 2003/06/30 18:05:32 simonet Exp $ *)

open Printf



(***************************************************************************)

let colors =
  [| Avl_graphics.yellow; Avl_graphics.white; Avl_graphics.red; Avl_graphics.green; 
     Avl_graphics.cyan; Avl_graphics.magenta; Avl_graphics.blue |]

let dot =
  `Dot [`Nodesep 20; `Ranksep 20]

type t =
    { matrix: bool array array;
      transient: int array;
      scc: (int * t) Avl_kernel.scc array;
      tarjan: int array
    }

let create n =
  { matrix = Array.make_matrix n n false;
    transient = Array.make n 0;
    scc = Array.make n (Avl_kernel.scc ());
    tarjan = Array.make n 0
  }


module GraphImpl = struct

  type graph = t
  and node = int * graph
  and edge = int * int * graph

  let iter_nodes f g =
    let n = Array.length g.matrix in
    for i = 0 to n - 1 do
      f (i, g)
    done

  let iter_edges f g =
    let n = Array.length g.matrix in
    for i = 0 to n - 1 do
      for j = 0 to n - 1 do
	if g.matrix.(i).(j) then f (i, j, g)
      done
    done

  let node_hash (i, _) = i
  let node_equal (i1, _) (i2, _) = i1 = i2

  let node_attributes (i, g) =
    { Avl_draw.default_node with
      Avl_draw.nd_label = 
      `Text {Avl_draw.default_label with Avl_draw.tl_text = string_of_int i};
      Avl_draw.nd_shape = `Circle;
      Avl_draw.nd_border = `Solid (Avl_graphics.blue, 0);
      Avl_draw.nd_background = 
      `Solid (colors.(g.tarjan.(i) mod Array.length colors))
    }

  let edge_hash (i, j, _) = Hashtbl.hash (i, j)
  let edge_equal (i, j, _) (i', j', _) = i = i' && j = j'
  let edge_head (i, _, g) = (i, g)
  let edge_tail (_, j, g) = (j, g)

  let draw_scc = ref false
  let edge_attributes (i, j, g) = 
    let c = 
      if !draw_scc then 
	begin if g.tarjan.(i) = g.tarjan.(j) 
	then Avl_graphics.rgb 128 128 128
	else Avl_graphics.black
	end
      else Avl_graphics.blue
    in
    { Avl_draw.default_edge with
      Avl_draw.ed_linestyle = `Solid (c, 0)
    }

  let iter_successors f (i, g) =
    let gi = g.matrix.(i) in
    for j = 0 to Array.length gi - 1 do
      if gi.(j) then f (j, g)
    done

  let get (i, g) =
    g.transient.(i)

  let set (i, g) k =
    g.transient.(i) <- k

  let get_scc (i, g) =
    g.scc.(i)

  let set_scc (i, g) scc =
    g.scc.(i) <- scc

end



(***************************************************************************)

let create_graph n m simple reflexive =

  let g = create n in

  for i = 1 to m do
    let rec random () =
      let i = Random.int n
      and j = Random.int n
      in
      if 
	(i <> j || reflexive)
	  &&
	(not g.matrix.(i).(j) or not simple)
      then g.matrix.(i).(j) <- true
      else random ()
    in
    random ()
  done;

  g



(***************************************************************************)

module Draw = Avl_draw.MakeGraphics (GraphImpl)
module Tarjan = Avl_tarjan.Make (GraphImpl)
module Closure = Avl_closure.Make (GraphImpl)
module Kernel = Avl_kernel.Make (GraphImpl)

let kernel g =
  let g' = create (Array.length g.matrix) in
  let add (i, _) (j, _) x = g'.matrix.(i).(j) <- true; x in
  ignore (Kernel.fold () add g);
  g'

let closure g =
  let g' = create (Array.length g.matrix) in
  let add (i, _) (j, _) x = g'.matrix.(i).(j) <- true; x in
  ignore (Closure.fold () add g);
  g'



let rec list_max_fst = function
    [] -> assert false
  | [x,_] -> x
  | (x,_) :: tl -> max x (list_max_fst tl)

let rec list_max_snd = function
    [] -> assert false
  | [_,x] -> x
  | (_,x) :: tl -> max x (list_max_snd tl)



let get_char () =
 (Avl_graphics.wait_next_event [Avl_graphics.Key_pressed]).Avl_graphics.key



let draw_menu sx sy text =
  let sizes = List.map Avl_graphics.text_size text in
  let w = list_max_fst sizes
  and h1 = list_max_snd sizes in

  let h = (h1 + 5) * List.length text - 5 in
  
  let x = sx / 2 + (sx / 2 - w) /2
  and y = (sy - h) /2 in

  Avl_graphics.set_color Avl_graphics.white;
  Avl_graphics.fill_rect (x - 5) (y - 5) (w + 10) (h + 10);
  Avl_graphics.set_color (Avl_graphics.rgb 0 0 64);
  Avl_graphics.draw_rect (x - 5) (y - 5) (w + 10) (h + 10);

  let y' =
    List.fold_right (fun s y' ->
      Avl_graphics.moveto x y';
      Avl_graphics.draw_string s;
      y' + h1 + 5
    ) (List.tl text) y;
  in
  Avl_graphics.set_color Avl_graphics.red;
  Avl_graphics.moveto x y';
  Avl_graphics.draw_string (List.hd text)



let main () =

  Random.init (int_of_float (Unix.time ()));

  Avl_graphics.open_graph " 800x600";
  Avl_graphics.auto_synchronize false;

  let sx = Avl_graphics.size_x ()
  and sy = Avl_graphics.size_y () in

  let draw_graph g x y sx sy title =
    ignore (Draw.draw_graph () dot (x + 10) (y + 10) g);
    let w, h = Avl_graphics.text_size title in
    Avl_graphics.moveto (x + (sx - w)/2) (y + sy - 10 - h);
    Avl_graphics.draw_string title;
    Avl_graphics.draw_rect
      (x + (sx - w)/2 - 5) (y + sy - 10 - h - 5) (w + 10) (h + 10)
  in

  let clean () =
    Avl_graphics.set_color Avl_graphics.white;
    Avl_graphics.fill_rect (sx/2) 0 sx sy;
    Avl_graphics.set_color Avl_graphics.black;
    Avl_graphics.set_line_width 0;
    Avl_graphics.moveto (sx/2) 0; Avl_graphics.lineto (sx/2) sy
  in

  let draw_result g title =
    clean ();
    draw_graph g (sx/2) 0 (sx/2) sy title
  in


  let g = ref (create 5) in
  let g_kernel = ref !g
  and g_closure = ref !g in

  let change_graph n m =
    g := create_graph n m true false;
    Avl_graphics.clear_graph ();
    draw_graph !g 0 0 (sx/2) sy "Original graph";
    ignore (Tarjan.fold 
	      [] (fun (i,_) tl -> i :: tl)
	      0 (fun list k ->
		List.iter (function i -> !g.tarjan.(i) <- k) list;
		succ k)
           !g);
    g_kernel := kernel !g;
    g_closure := closure !g
  in

  change_graph 7 12;

  let menu () =
    clean ();
    draw_menu sx sy
      [ "MAIN MENU";
	"Press one of the following keys to continue:";
	"(n) New graph";
	"(s) SCC (Tarjan's algorithm)";
	"(t) Transitive closure";
	"(k) Transitive reduction (kernel)";
	"(q) Quit"
      ]	
  in

  let new_graph () =
    let nodes = ref ""
    and edges = ref ""
    and message = ref "Enter the number of nodes and"
    and line = ref 0
    in
    clean ();
    let rec loop () =
      draw_menu sx sy
	[ "NEW GRAPH                    ";
	  sprintf "Nodes: %s%s" !nodes (if !line = 0 then "_" else "");
	  sprintf "Edges: %s%s" !edges (if !line = 1 then "_" else "");
	  "";
	  !message;
	  "press (enter) to continue.";
	  "press (q) to quit."
	];
      Avl_graphics.synchronize ();

      match get_char () with
	'0' .. '9' as c ->
	  begin match !line with
	    0 -> nodes := !nodes ^ (String.make 1 c); loop ()
	  | 1 -> edges := !edges ^ (String.make 1 c); loop ()
	  | _ -> ()
	  end
      | '\r' ->
	  incr line;
	  begin match !line with
	    1 ->
	      message := sprintf "Enter the number of edges and";
	      loop ()
	  | 2 ->
	      let n = int_of_string ("0" ^ !nodes)
	      and m = int_of_string ("0" ^ !edges) in
	      if n < 0 or m < 0 or m > n * (n - 1) then begin
		message := sprintf "Invalid numbers !";
		incr line;
		loop ()
	      end
	      else change_graph n m
	  | _ -> ()
	  end
      | 'q' -> ()
      | _ -> if !line < 2 then loop ()
    in
    loop ()
  in

  let rec loop () =
    Avl_graphics.synchronize ();
    begin 
      match get_char () with
      'q' -> exit 0
    | 'n' ->
	new_graph ();
	menu ()
    | 's' -> 
	GraphImpl.draw_scc := true;
	draw_result !g "SCC";
	GraphImpl.draw_scc := false;
    | 't' -> 
	draw_result !g_closure "Transitive closure";
    | 'k' -> 
	draw_result !g_kernel "Kernel"
    | _ -> 
	menu ()
    end;
    loop ()

  in

  menu ();
  loop ()



let () = main ()
