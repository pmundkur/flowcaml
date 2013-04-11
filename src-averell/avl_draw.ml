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

(* $Id: avl_draw.ml,v 1.5 2003/06/30 18:05:32 simonet Exp $ *)

(** Drawing graphs.

    This module allows drawing a graphical representation of directed 
    graphs.  Arbitrary output devices may be used, e.g. the Objective
    Caml graphics library. Arrangements of nodes and edges are computed
    thanks to the GraphViz tools.
 *)



let rec filter_map f = function
    [] -> []
  | hd :: tl ->
      match f hd with
	None -> filter_map f tl
      | Some hd' -> hd' :: filter_map f tl



(***************************************************************************)
(** {2 Drawing attributes}

    This sections defines datatype for specifying the drawing attributes
    of the nodes and edges of a graph.
 *)

(** Colors are represented by integers, using the same encoding than that
    of the Objective Caml graphics library.
 *)
type color = int

(** Coordinates of point are given by a pair of integers.
 *)
type point = int * int



(*-------------------------------------------------------------------------*)
(** {3 Nodes attributes} *)

(** The drawing attributes of a node are specified by a record of
    type ['a node_attributes].
 *)
type node_attributes =
    { nd_label: [ `None | `Text of text_label_attributes ];
        (** Sets the label to be drawed inside the node.  Two options are
	    currently implemented: 
	    - [`None] : no label,
	    - [`Text att] : textual label, described by [att].
	 *)
      nd_shape: [ `Box | `Square | `Circle | `Ellipse ];
        (** Sets the shape of the border of the node.  Available shapes
	    are [`Box] (rectangle), [`Square], [`Circle] and [`Ellipse]. *)
      nd_size: [ `FitLabel of int * int | `Fixed of int * int ];
        (** Sets the the size of the label.  Two options are available:
	    - [`FitLabel (hm, vm)]: the shape will fit the label, with
	      horizontal and vertical margin of [hm] and [vm] pixels,
	      respectively.
	    - [`Fixed (w, h)]: the label will fit a rectangle of witdth [w]
	      and height [h] pixels.
	 *)
      nd_border: [`NoBorder | `Solid of (color * int)];
        (** Sets the style of the border.  Available styles are:
	    - [`NoBorder]: no border will be drawn.
	    - [`Solid (c, w)]: a solid border will be drawn with color
	      [c] and width [w].
	 *)
      nd_background: [`Transparent | `Solid of color]
	(** Sets the style of the background of the node.  Available styles
	    are:
	    - [`Transparent]: no background.
	    - [`Solid c]: the node will be filled with color [c].
	 *)
    } 

(** The attributes of a text label are specified by a record of type
    [text_label_attributes]. *)
and text_label_attributes =
    { tl_text: string;
        (** The litteral text of the label. *)
      tl_fontname: string;
        (** The family font name.  If the string is empty, default system
	    font will be kept. *)
      tl_fontsize: int;
        (** The font size. *)
      tl_color: color
	(** The font color. *)
    } 

(** [default_node] is a standard record of node attributes. 
 *)
let default_node =
  { nd_label = `None;
    nd_shape = `Box;
    nd_size = `FitLabel (10, 10);
    nd_border = `Solid (Avl_graphics.black, 0);
    nd_background = `Solid Avl_graphics.white
  } 

(** [default_text_label] is a standard record of text label attributes.
 *)
let default_label =
  { tl_text = "";
    tl_fontname = "";
    tl_fontsize = 10;
    tl_color = Avl_graphics.black
  } 



(*-------------------------------------------------------------------------*)
(** {3 Edges attributes} *)

(** The drawing attributes of an edge are specified by a record of type
    [edge_attributes].  
 *)
type edge_attributes =
    { ed_linestyle: [ `Transparent | `Solid of color * int ];
        (** Sets the drawing style of the edge line.  Available options are:
	    - [`Transparent]: no line will be traced.
	    - [`Solid (c, w)]: a solid line will be drawn with color
	      [c] and width [w].
	 *)
      ed_originarrow: arrow_style;
        (** The style of the origin arrow. *)
      ed_tailarrow: arrow_style;
        (** The style of the tail arrow. *)
    } 

(** Available arrows styles: *)
and arrow_style =
  [ `None
      (** No arrow. *)
  | `Lined of int * float * color * int
      (** A two-lines arrow. *)
  | `Filled of int * float * color
      (** A filled triangular. *)
  ] 

(** [default_edge] is a standard record of edge attributes. 
 *)
let default_edge =
  { ed_linestyle = `Solid (Avl_graphics.black, 0);
    ed_originarrow = `None;
    ed_tailarrow = `Filled (10, 0.5 *. atan 1.0, Avl_graphics.black)
  } 



(***************************************************************************)
(** {2 Client's signatures} *)

(** The client must provide an implementation of graphs which fullfills the
    signature [GRAPH].
 *)
module type GRAPH = sig

  type graph
  type node
  type edge

  val iter_nodes: (node -> unit) -> graph -> unit
  val iter_edges: (edge -> unit) -> graph -> unit

  val node_hash: node -> int
  val node_equal: node -> node -> bool
  val node_attributes: node -> node_attributes

  val edge_hash: edge -> int
  val edge_equal: edge -> edge -> bool
  val edge_head: edge -> node
  val edge_tail: edge -> node
  val edge_attributes: edge -> edge_attributes

end



(** The client must provide drawing functions which fullfills the
    signature [DRAW].
 *)
module type DRAW = sig

  type window

  val draw_lines: window -> color:color -> lw:int -> point list -> unit
  val draw_curves: window -> color:color -> lw:int 
    -> point -> (point * point * point) list -> unit
  val draw_rect: window ->
      color:color -> lw:int -> x:int -> y:int -> w:int -> h:int -> unit
  val draw_ellipse: window ->
      color:color -> lw:int -> x:int -> y:int -> rx:int -> ry:int -> unit

  val fill_rect: window ->
      color:color -> x:int -> y:int -> w:int -> h:int -> unit
  val fill_ellipse: window ->
      color:color -> x:int -> y:int -> rx:int -> ry:int -> unit
  val fill_poly: window -> color:color -> point list -> unit

  val draw_text: window -> color:color -> ?name:string -> size:int
    -> x:int -> y:int -> string -> unit
  val text_size: window -> ?name:string -> size:int -> string -> int * int

end



(** [DrawGraphics] provides an implementation of signature [DRAW] for
    the graphics library of Objective Caml.
 *)
module DrawGraphics : DRAW with type window = unit = struct

  open Avl_graphics

  type window = unit
  
  let draw_lines () ~color ~lw = function
      [] -> ()
    | (x0, y0) :: points ->
	set_color color;
	set_line_width lw;
	moveto x0 y0;
	List.iter (function x, y -> lineto x y) points

  let draw_curves () ~color ~lw (x0, y0) points =
    set_color color;
    set_line_width lw;
    moveto x0 y0;
    List.iter (function p0, p1, p2 -> curveto p0 p1 p2) points

  let draw_rect () ~color ~lw ~x ~y ~w ~h =
    set_color color;
    set_line_width lw;
    draw_rect x y w h

  let draw_ellipse () ~color ~lw ~x ~y ~rx ~ry =
    set_color color;
    set_line_width lw;
    draw_ellipse x y rx ry

  let fill_rect () ~color ~x ~y ~w ~h =
    set_color color;
    fill_rect x y w h

  let fill_ellipse () ~color ~x ~y ~rx ~ry =
    set_color color;
    fill_ellipse x y rx ry

  let fill_poly () ~color points =
    set_color color;
    fill_poly (Array.of_list points)

  let draw_text () ~color ?name ~size ~x ~y text =
    set_color color;
    set_text_size size;
    moveto x y;
    begin match name with
      None -> ()
    | Some name' -> set_font name'
    end;
    draw_string text

  let text_size () ?name ~size text =
    set_text_size size;
    begin match name with
      None -> ()
    | Some name' -> set_font name'
    end;
    text_size text

end



(***************************************************************************)
(** {2 [Make] functor} *)

(** Given an implementation of drawing capabilities and of graphs structures,
    the functor [Make] provide drawing functions for graphs.
 *)
module Make (D: DRAW) (G: GRAPH) = struct

  (*-----------------------------------------------------------------------*)
  (** {3 Engines} *)

  type engine =
    [ `Dot of 
        [ `Nodesep of int
        | `Ranksep of int
        | `Rankdir of [`TopToBottom | `LeftToRight]
        ] list
(*
    | `Neato of
	[ `Spline of bool
        | `Overlap of bool
	| `Start of int
	| `Nodepos of G.node -> (int * int)
	| `Edgelen of G.edge -> int
	] list
*)
    ] 



  (*-----------------------------------------------------------------------*)
  (** {3 Drawing nodes} *)

  (** [label_size label] returns the couple of dimensions of the drawing 
      of a label described by [label].
   *)
  let label_size win = function
      `None -> 0, 0
    | `Text lbl ->
	D.text_size win
	  ?name:(if lbl.tl_fontname = "" then None else Some lbl.tl_fontname)
	  ~size:lbl.tl_fontsize lbl.tl_text



  (** [shape_size w h shape] returns the minimum dimensions of a node of
      shape [shape] which may contain a label whose width and height are
      [w] and [h], respectively.
   *)
  let shape_size w h = function
      `Box -> w, h
    | `Square -> let a = max w h in a, a
    | `Circle ->
	let sq i = i * i 
	and isq i = int_of_float (ceil (sqrt (float_of_int i))) in
	let r = isq (sq w + sq h) in
	r, r
    | `Ellipse ->
	let f x = int_of_float (ceil (float_of_int x *. sqrt 2.0)) in
	f w, f h



  (** [node_size win nd] returns the couple of dimensions of the drawing of 
      a node of attributes [nd].
   *)
  let node_size win nd =
    match nd.nd_size with
      `FitLabel (hm, vm) ->
	let wl, hl = label_size win nd.nd_label in
	shape_size (wl + hm) (hl + vm) nd.nd_shape
    | `Fixed (w, h) -> 
	w, h

  (** [draw_node win nd x y] draws a node on window [win]. 
      [x] and [y] are the coordinates of the center of the node.  
      [nd] specifies the attributes for the drawing.
   *)
  let draw_node win nd x y =

    let w, h = node_size win nd in

    (* Background *)
    begin match nd.nd_background with
      `Transparent -> ()
    | `Solid color ->
	match nd.nd_shape with
	  `Box | `Square -> 
	    D.fill_rect win ~color ~x:(x - w/2) ~y:(y - h/2) ~w ~h
	| `Circle | `Ellipse -> 
	    D.fill_ellipse win ~color ~x ~y ~rx:(w/2) ~ry:(h/2)
    end;

    (* Foreground *)
    begin match nd.nd_border with
      `NoBorder -> ()
    | `Solid (color, lw) ->
	match nd.nd_shape with
	  `Box | `Square -> 
	    D.draw_rect win ~color ~lw ~x:(x - w/2) ~y:(y - h/2) ~w ~h
	| `Circle | `Ellipse -> 
	    D.draw_ellipse win ~color ~lw ~x ~y ~rx:(w/2) ~ry:(h/2)
    end;

    (* Label *)
    begin match nd.nd_label with
      `None -> ()
    | `Text lbl ->
	let name = if lbl.tl_fontname = "" then None else Some lbl.tl_fontname in
	let wl, hl = D.text_size win ?name ~size:lbl.tl_fontsize lbl.tl_text in
	D.draw_text win ~color:lbl.tl_color
	  ?name ~size:lbl.tl_fontsize ~x:(x - wl/2) ~y:(y - hl/2) lbl.tl_text
    end



  (*-------------------------------------------------------------------------*)
  (** {3 Drawing edges} *)
    
  let round f =
    int_of_float (f +. 0.5)

  let pi =
    4.0 *. atan 1.0



  (** [draw_arrow win x y angle style] draw an arrow of style [style] on
      window [win].
      [x] and [y] are the coordinate of the tip of the arrow.  [angle]
      gives the direction of the arrow by specifiying its angle [in
      radians] with the [x]-axis.
   *)
  let draw_arrow win x y angle style =

    let coords radius rotation =
      let _r = float_of_int radius in
      round ((float_of_int x) +. _r *. cos (pi +. angle +. rotation)),
      round ((float_of_int y) +. _r *. sin (pi +. angle +. rotation))
    in

    match style with
      `None -> ()
    | `Lined (length, opening, color, lw) ->
	let x1, y1 = coords length opening
	and x2, y2 = coords length (-. opening) in
	D.draw_lines win ~color ~lw [x1, y1; x, y; x2, y2]
    | `Filled (length, opening, color) -> 
	let x1, y1 = coords length opening
	and x2, y2 = coords length (-. opening) in
	D.fill_poly win ~color [x1, y1; x, y; x2, y2]



  type path = point * (point * point * point) list



  let path_rev (p, points) = 
    let rec path_rev_rec (p, accu) = function
	[] -> (p, accu)
      | (p1, p2, p3) :: tail ->
	  path_rev_rec (p3, (p2, p1, p) :: accu) tail
    in
    path_rev_rec (p, []) points



  let rec path_translate h v (origin, points) = 
    let f (x, y) = (x + h, y + v) in
    f origin, List.map (function p0, p1, p2 -> f p0, f p1, f p2) points



  let path_origin = function
      (x0, y0), [] -> x0, y0, 0.0
    | (x0, y0), ((x1, y1), _, _) :: _ ->
	x0, y0, atan2 (float_of_int (y0 - y1)) (float_of_int (x0 - x1))



  let path_tail ((x0, y0), points) =
    let rec path_tail_rec = function
	[] -> x0, y0, 0.0
      | [_, (x1, y1), (x0, y0)] ->
	  x0, y0, atan2 (float_of_int (y0 - y1)) (float_of_int (x0 - x1))
      | _ :: tail -> path_tail_rec tail
    in
    path_tail_rec points



  (** [draw_edge ed path] draw an edge along path [path] on window [win], 
      using the attributes given by [ed]. *)
  let draw_edge win ed ((origin, points) as path) =

    (* Drawing the line. *)
    begin match ed.ed_linestyle with
      `Transparent -> ()
    | `Solid (color, lw) ->
	D.draw_curves win ~color ~lw origin points
    end;

    (* Origin arrow. *)
    let x0, y0, angle0 = path_origin path in
    draw_arrow win x0 y0 angle0 ed.ed_originarrow;

    (* Tail arrow. *)
    let xt, yt, anglet = path_tail path in
    draw_arrow win xt yt anglet ed.ed_tailarrow



  (*-----------------------------------------------------------------------*)
  (** {3 Arrangement} *)

  module IntTbl = Hashtbl.Make (struct
    type t = int
    let hash : int -> int = Hashtbl.hash
    let equal : int -> int -> bool = (=)
  end)

  module NodeTbl = Hashtbl.Make (struct
    type t = G.node
    let hash = G.node_hash
    let equal = G.node_equal
  end)

  module EdgeTbl = Hashtbl.Make (struct
    type t = G.edge
    let hash = G.edge_hash
    let equal = G.edge_equal
  end)

  type arrangement =
      { nodes: (int * int) NodeTbl.t;
	edges: path EdgeTbl.t;
	mutable width: int;
	mutable height: int;
	mutable scalefactor: float
      }	

  type index_ng =
      { idx_nodes: int NodeTbl.t;
	idx_edges: int EdgeTbl.t
      }	



  (*-----------------------------------------------------------------------*)
  (** (3 Parsing dot output} *)

  exception Error of string
  exception Dot_input_error of int * string * string

  (** [split s] splits a string into a list of substrings.  Separation is
      made at each occurence of ' '.
   *)
  let split s =

    let len = String.length s in

    let rec split_rec i =
      try
	let i' = String.index_from s i ' ' in
	if i' = i then split_rec (i' + 1) 
	else String.sub s i (i' - i) :: split_rec (i' + 1)
      with 
	Not_found ->
	  if len = i then [] else [String.sub s i (len - i)]
    in

    split_rec 0



  let read_float s =
    try 
      float_of_string s
    with
      Failure _ -> raise (Error "Unparsable floating point number")

  let read_int_float s =
    try 
      round (float_of_string s)
    with
      Failure _ -> raise (Error "Unparsable floating point number")

  let read_int s =
    try 
      int_of_string s
    with
      Failure _ -> raise (Error "Unparsable integer")

  let read_int_color s =
    try
      int_of_string ("0x" ^ (String.sub s 1 (String.length s - 1)))
    with
      Failure _ | Invalid_argument _ -> raise (Error "Unparsable color")

  let rec read_path n path =
    match n, path with
      1, _x :: _y :: tail ->
	((read_int_float _x, read_int_float _y), []), tail
    | n, _x1 :: _y1 :: _x2 :: _y2 :: _x3 :: _y3 :: tail ->
	let (origin, points), tail' = read_path (n - 3) tail in
	((read_int_float _x1, read_int_float _y1),
	 ((read_int_float _x2, read_int_float _y2),
	  (read_int_float _x3, read_int_float _y3), origin) :: points), tail'
  | _ -> raise (Error "Unparsable edge path")



  (*-----------------------------------------------------------------------*)
  (** {3 Interface with GraphViz} *)

  let current_window : D.window option ref = ref None
  let win () = 
    match !current_window with
      None -> assert false
    | Some win -> win

  module G' = struct

    type node = int * G.node
    type edge = int * G.edge * node * node

    let iter_nodes f (nodes, _, _) =
      IntTbl.iter (fun i node -> f (i, node)) nodes

    let iter_edges f (_, edges, _) =
      IntTbl.iter (fun i (edge, h, t) -> f (i, edge, h, t)) edges

    let node_name (i, _) = string_of_int i

    let edge_head (_, _, h, _) = h
    let edge_tail (_, _, _, t) = t

    let graph_attributes (_, _, attributes) =
      attributes

    let common_node_attributes node =
      let nd = G.node_attributes node in
      let shape = 
	match nd.nd_shape with
	  `Box | `Square -> `Box
	| `Circle -> `Circle
	| `Ellipse -> `Ellipse
      in
      let w, h = node_size (win ()) nd in
      [`Shape shape; `Width (float_of_int w); `Height (float_of_int h)]

  end

  module Dot = Avl_graphviz.Dot.Make (struct

    include G'

    type graph =
	G.node IntTbl.t * (G.edge * node * node) IntTbl.t
	  * Avl_graphviz.Dot.Attributes.graph list

    let default_node_attributes _ = [`Fixedsize true; `Label ""]
    let default_edge_attributes _ = [`Dir `None]

    let node_attributes (_, node) =
      common_node_attributes node

    let edge_attributes (i, edge, _, _) =
      [`Color i]

  end)




  let neato_nodepos =
    ref (function node -> raise Not_found)

  let neato_edgelen = 
    ref (function edge -> raise Not_found)

  module Neato = Avl_graphviz.Neato.Make (struct

    include G'

    type graph =
	G.node IntTbl.t * (G.edge * node * node) IntTbl.t
	  * Avl_graphviz.Neato.Attributes.graph list

    let default_node_attributes _ = [`Label ""]
    let default_edge_attributes _ = [`Dir `None]

    let node_attributes (_, node) =
      try 
	let x, y = (! neato_nodepos) node  in
	(`Pos (float_of_int x, float_of_int y)) :: common_node_attributes node
      with
	Not_found -> common_node_attributes node

    let edge_attributes (i, edge, _, _) =
      try
	let len = (! neato_edgelen) edge in
	(`Len (float_of_int len)) :: [`Color i]
      with
	Not_found -> [`Color i]

  end)



  let arrange win engine graph =

    current_window := Some win;

    (** Graph indexing. *)
    
    let index_nodes = IntTbl.create 7
    and index_edges = IntTbl.create 7
    and nodes_index = NodeTbl.create 7
    in

    let node_counter = ref 0 in
    G.iter_nodes (function node ->
      incr node_counter;
      IntTbl.add index_nodes !node_counter node;
      NodeTbl.add nodes_index node !node_counter
    ) graph;

    let edge_counter = ref 0 in
    G.iter_edges (function edge ->
      incr edge_counter;
      let h = G.edge_head edge
      and t = G.edge_tail edge in
      try
	IntTbl.add index_edges !edge_counter
	  (edge, (NodeTbl.find nodes_index h, h), 
	   (NodeTbl.find nodes_index t, t))
      with
	Not_found -> assert false
    ) graph;


    (** Computing the arrangement *)

    let arr =
      { nodes = NodeTbl.create 7;
	edges = EdgeTbl.create 7;
	width = 0;
        height = 0;
        scalefactor = 0.0
      }
    in

    let read_plain ic =

      let rec loop linenumber =
	let line = 
	  try input_line ic
	  with End_of_file ->
	    raise (Dot_input_error(linenumber, "", "End of output"))
	in
	try match split line with
	  "stop" :: _ -> ()
	| "graph" :: sf :: w :: h :: _ -> 
	    arr.width <- read_int_float w;
	    arr.height <- read_int_float h;
	    arr.scalefactor <- read_float sf;
	    loop (linenumber + 1)
	| "node" :: id :: x :: y :: _ ->
	    let node = 
	      try IntTbl.find index_nodes (read_int id) 
	      with Not_found -> assert false
	    in
	    NodeTbl.add arr.nodes node (read_int_float x, read_int_float y);
	    loop (linenumber + 1)
	| "edge" :: _ :: _ :: n :: list ->
	    begin match read_path (read_int n) list with
	      path, _ :: id :: _ -> 
		let x0, y0, _ = path_origin path in
		let edge, _, _ = 
		  try IntTbl.find index_edges (read_int_color id) 
		  with Not_found -> assert false
		in
		let xh, yh = 
		  try NodeTbl.find arr.nodes (G.edge_head edge)
		  with Not_found -> assert false
		and xt, yt = 
		  try NodeTbl.find arr.nodes (G.edge_tail edge)
		  with Not_found -> assert false
		in
		let path' =
		  if abs (xh-x0) + abs (yh-y0) < abs (xt-x0) + abs (yt-y0)
		  then path else path_rev path
		in
		EdgeTbl.add arr.edges edge path';
		loop (linenumber + 1)
	    | _ -> raise (Error "Incomplete edge line")
	    end
	| _ -> raise (Error "Illegal line format")

	with
	  Error msg -> raise (Dot_input_error (linenumber, line, msg))

      in
      loop 1

    in

    begin match engine with
      `Dot options ->
	let options' =
	  List.map (function
	      `Nodesep i -> `Nodesep (float_of_int i)
	    | `Ranksep i -> `Ranksep (float_of_int i)
	    | (`Rankdir _) as a -> a
          ) options
	in
	Dot.run_graph `Plain read_plain (index_nodes, index_edges, options');
    | `Neato options ->
	neato_nodepos := (function node -> raise Not_found);
	neato_edgelen := (function edge -> raise Not_found);
	Avl_graphviz.Neato.set_command "neato"; (* TEMPORARY *)
	let options' = 
	  filter_map (function
	      (`Spline _ | `Overlap _ | `Start _) as a -> Some a
	    | `Nodepos f -> 
		Avl_graphviz.Neato.set_command "neato -s -n"; (* TEMPORARY *)
		neato_nodepos := f; None
	    | `Edgelen f -> neato_edgelen := f; None
          ) options
	in
	Neato.run_graph `Plain read_plain (index_nodes, index_edges, options')
    end;

    arr


  let draw_graph win engine x0 y0 graph =

    let arr = arrange win engine graph in

    flush stderr;

    current_window := Some win;

    G.iter_edges (function edge ->
      let path = 
	try EdgeTbl.find arr.edges edge
	with Not_found -> assert false
      in
      draw_edge win (G.edge_attributes edge) (path_translate x0 y0 path)
    ) graph;

    G.iter_nodes (function node ->
      let x, y = 
	try NodeTbl.find arr.nodes node
	with Not_found -> assert false
      in
      draw_node win (G.node_attributes node) (x + x0) (y + y0)
    ) graph;

    arr.width, arr.height


end



(** Given an implementation of graphs structures, the functor
    [MakeGraphics] provides drawing functions for the Objective Caml
    graphics library.
 *)
module MakeGraphics (G: GRAPH) =
  Make (DrawGraphics) (G)
