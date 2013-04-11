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

(* $Id: avl_draw.mli,v 1.4 2003/06/26 13:32:45 simonet Exp $ *)

(** Drawing graphs.

    This module allows drawing a graphical representation of directed 
    graphs.  Arbitrary output devices may be used, e.g. the Objective
    Caml graphics library. Arrangements of nodes and edges are computed
    thanks to the GraphViz tools.
 *)



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
val default_node: node_attributes

(** [default_text_label] is a standard record of text label attributes.
 *)
val default_label: text_label_attributes



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
val default_edge: edge_attributes



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
module DrawGraphics : DRAW



(***************************************************************************)
(** {2 [Make] functor} *)

(** Given an implementation of drawing capabilities and of graphs structures,
    the functor [Make] provide drawing functions for graphs.
 *)
module Make (D: DRAW) (G: GRAPH) : sig

  (** Spatial arrangement of nodes and edges is performed by an
      external engine.  Currently, only one external engine is supported.
   *)
  type engine =
    [ `Dot of 
        [ `Nodesep of int
        | `Ranksep of int
        | `Rankdir of [`TopToBottom | `LeftToRight]
        ] list
      (** The parameter is a list of options, allowing two set:
	  - the minimal distance between two nodes of the same rank (`Nodesep),
	  - the minimal distance between two nodes of different ranks
	    (`Ranksep),
	  - the orientation of ranks (`Rankdir)
       *)
    ] 

  (** [draw_graph win a x y g] draw the graph [g] according to the arrangement
      [a].  [x] and [y] are the coordinates of the left lower bound of the
      drawing.  The function returns the dimensions of the drawing.
   *)
  val draw_graph: D.window -> engine -> int -> int -> G.graph -> (int * int)

end



(** Given an implementation of graphs structures, the functor
    [MakeGraphics] provides drawing functions for the Objective Caml
    graphics library.  See {!Make} for documentation.
 *)
module MakeGraphics (G: GRAPH) : sig

  type engine =
    [ `Dot of 
        [ `Nodesep of int
        | `Ranksep of int
        | `Rankdir of [`TopToBottom | `LeftToRight]
        ] list
    ] 

  val draw_graph: unit -> engine -> int -> int -> G.graph -> (int * int)

end
