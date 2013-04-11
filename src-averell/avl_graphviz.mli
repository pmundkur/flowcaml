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

(* $Id: avl_graphviz.mli,v 1.4 2003/06/26 13:32:45 simonet Exp $ *)

(** Interface with {i GraphViz}

    This module provides a basic interface with dot and neato,
    two programs of the GraphViz toolbox.
    These tools are available at the following URLs:

    {v http://www.graphviz.org/ v}

    {v http://www.research.att.com/sw/tools/graphviz/ v}

 *)

open Format



(***************************************************************************)
(** {2 Common stuff} *)

(** Because the neato and dot engines present a lot of common points -
    in particular in the graph description language, large parts of
    the code is shared.  First, the [!CommonAttributes] module defines
    attributes of graphs, nodes and edges that are understood by the
    two engines.  Second, given a module (of type [!ENGINE])
    describing an engine the [!MakeEngine] functor provides suitable
    interface function for it. *)

(*-------------------------------------------------------------------------*)
(** {3 Common attributes} *)

type color = int

type arrow_style =
  [ `None | `Normal | `Inv | `Dot | `Odot | `Invdot | `Invodot ] 



(** The [CommonAttributes] module defines attributes for graphs, nodes and edges
    that are available in the two engines, dot and neato.
 *)
module CommonAttributes : sig

  (** Attributes of graphs.
   *)
  type graph =
    [ `Center of bool
        (** Centers the drawing on the page.  Default value is [false]. *)
    | `Fontcolor of color
        (** Sets the font color.  Default value is [black]. *)
    | `Fontname of string
        (** Sets the font family name.  Default value is ["Times-Roman"]. *)
    | `Fontsize of int
        (** Sets the type size (in points).  Default value is [14]. *)
    | `Label of string
        (** Caption for graph drawing. *)
    | `Orientation of [ `Portrait | `Landscape ]
        (** Sets the page orientation.  Default value is [`Portrait]. *)
    | `Page of float * float
        (** Sets the PostScript pagination unit, e.g [8.5, 11.0]. *)
    | `Pagedir of [ `TopToBottom | `LeftToRight ]
        (** Traversal order of pages.  Default value is [`TopToBottom]. *)
    | `Size of float * float
        (** Sets the bounding box of drawing (in inches). *)
    ] 

  (** Attributes of nodes.
   *)
  type node =
    [ `Color of color
        (** Sets the color of the border of the node. Default value is [black]
         *)
    | `Fontcolor of color
        (** Sets the label font color.  Default value is [black]. *)
    | `Fontname of string
        (** Sets the label font family name.  Default value is
            ["Times-Roman"]. *)
    | `Fontsize of int
        (** Sets the label type size (in points).  Default value is [14].
         *)
    | `Height of float
        (** Sets the minimum height.  Default value is [0.5]. *)
    | `Label of string
        (** Sets the label printed in the node. The string may include escaped
            newlines \n, \l, or \r for center, left, and right justified lines.
            Record labels may contain recursive box lists delimited by { | }. *)
    | `Orientation of float
        (** Node rotation angle, in degrees.  Default value is [0.0]. *)
    | `Peripheries of int
        (** Sets  the  number  of periphery lines drawn around the polygon. *)
    | `Regular of bool
        (** If [true], then the polygon is made regular, i.e. symmetric about
	    the x and y axis, otherwise  the polygon   takes   on   the  aspect
	    ratio of the label.  Default value is [false]. *)
    | `Shape of
        [`Ellipse | `Box | `Circle | `Doublecircle | `Diamond
        | `Plaintext | `Record | `Polygon of int * float]
        (** Sets the shape of the node.  Default value is [`Ellipse].
            [`Polygon (i, f)] draws a polygon with [n] sides and a skewing
            of [f]. *)
    | `Style of [ `Filled | `Solid | `Dashed | `Dotted | `Bold | `Invis ]
        (** Sets the layout style of the node.  Several styles may be combined
            simultaneously. *)
    | `Width of float
        (** Sets the minimum width.  Default value is [0.75]. *)
    ]     

  (** Attributes of edges.
   *)
  type edge =
    [ `Color of color
        (** Sets the edge stroke color.  Default value is [black]. *)
    | `Decorate of bool
        (** If [true], draws a line connecting labels with their edges. *)
    | `Dir of [ `Forward | `Back | `Both | `None ] 
        (** Sets arrow direction.  Default value is [`Forward]. *)
    | `Fontcolor of color
        (** Sets the label font color.  Default value is [black]. *)
    | `Fontname of string
        (** Sets the label font family name.  Default value is
	    ["Times-Roman"]. *)
    | `Fontsize of int
        (** Sets the label type size (in points).  Default value is [14]. *)
    | `Label of string
        (** Sets the label to be attached to the edge.  The string may include
	    escaped newlines \n, \l, or \r for centered, left, or right
	    justified lines. *)
    | `Labelfontcolor of color
        (** Sets the font color for head and tail labels.  Default value is
            [black]. *)
    | `Labelfontname of string
        (** Sets the font family name for head and tail labels.  Default
            value is ["Times-Roman"]. *)
    | `Labelfontsize of int
        (** Sets the font size for head and tail labels (in points). 
            Default value is [14]. *)
    | `Style of [ `Solid | `Dashed | `Dotted | `Bold | `Invis ]
        (** Sets the layout style of the edge.  Several styles may be combined
            simultaneously. *)
    ]     

end



(***************************************************************************)
(** {2 Interface with the dot engine} *)

module Dot : sig

  (** Several functions provided by this module run the external program
      {i dot}.  By default, this command is supposed to be in the default
      path and is invoked by {i dot}.  The function
      [set_command] allows to set an alternative path at run time.
   *)
  val set_command: string -> unit

  module Attributes : sig

  (** Attributes of graphs.  They include all common graph attributes and
      several specific ones.  All attributes described in the "dot User's
      Manual, February 4, 2002" are handled, excepted: clusterank, color,
      compound, labeljust, labelloc, ordering, rank, remincross, rotate,
      searchsize and style.
   *)
  type graph =
    [ CommonAttributes.graph
    | `Bgcolor of color
        (** Sets the background color and the inital fill color. *)
    | `Comment of string
        (** Comment string. *)
    | `Concentrate of bool
        (** If [true], enables edge concentrators.  Default value is [false]. *)
    | `Fontpath of string
        (** List of directories for fonts. *)
    | `Layers of string list
        (** List of layers. *)
    | `Margin of float
        (** Sets the page margin (included in the page size).  Default value is
            [0.5]. *)
    | `Mclimit of float
        (** Scale factor for mincross iterations.  Default value is [1.0]. *)
    | `Nodesep of float
        (** Sets the minimum separation between nodes, in inches.  Default
            value is [0.25]. *)
    | `Nslimit of int
        (** If set of [f], bounds network simplex iterations by [f *
            <number of nodes>] when ranking nodes. *)
    | `Nslimit1 of int
        (** If set of [f], bounds network simplex iterations by [f *
            <number of nodes>] when setting x-coordinates. *)
    | `Ranksep of float
        (** Sets the minimum separation between ranks. *)
    | `Quantum of float
        (** If not [0.0], node label dimensions will be rounded to integral
	    multiples of it.  Default value is [0.0]. *)
    | `Rankdir of [ `TopToBottom | `LeftToRight ]
        (** Direction of rank ordering.  Default value is [`TopToBottom]. *)
    | `Ratio of [ `Float of float | `Fill | `Compress| `Auto ]
        (** Sets the aspect ratio. *)
    | `Samplepoints of int
        (** Number of points used to represent ellipses and circles on output.
	    Default value is [8]. *)
    | `Url of string
        (** URL associated with graph (format-dependent). *)
    ] 

  (** Attributes of nodes.  They include all common node attributes and
      several specific ones.  All attributes described in the "dot User's
      Manual, February 4, 2002" are handled, excepted: bottomlabel, group,
      shapefile and toplabel.
   *)
  type node =
    [ CommonAttributes.node
    | `Comment of string
        (** Comment string. *)
    | `Distortion of float
        (* TEMPORARY *)
    | `Fillcolor of color
        (** Sets the fill color (used when `Style filled).  Default value
            is [lightgrey]. *)
    | `Fixedsize of bool
        (** If [true], forces the given dimensions to be the actual ones.
            Default value is [false]. *)
    | `Layer of string
        (** Overlay. *)
    | `Url of string
        (** The  default  url  for  image  map  files; in PostScript files,
            the base URL for all relative URLs, as recognized by Acrobat
	    Distiller 3.0 and up. *)
    | `Z of float
        (** z coordinate for VRML output. *)
    ] 

  (** Attributes of edges.  They include all common edge attributes and
      several specific ones.  All attributes described in the "dot User's
      Manual, February 4, 2002" are handled, excepted: lhead and ltail.
   *)
  type edge =
    [ CommonAttributes.edge
    | `Arrowhead of arrow_style
        (** Sets the style of the head arrow.  Default value is [`Normal]. *)
    | `Arrowsize of float
        (** Sets the scaling factor of arrowheads.  Default value is [1.0]. *)
    | `Arrowtail of arrow_style
        (** Sets the style of the tail arrow.  Default value is [`Normal]. *)
    | `Comment of string
        (** Comment string. *)
    | `Constraints of bool
        (** If [false], causes an edge to be ignored for rank assignment. 
            Default value is [true]. *)
    | `Headlabel of string
        (** Sets the label attached to the head arrow. *)
    | `Headport of [ `N | `NE | `E | `SE | `S | `SW | `W | `NW ]
        (* TEMPORARY *)
    | `Headurl of string
        (** Url attached to head label if output format is ismap. *)
    | `Labelangle of float
        (** Angle in degrees which head or tail label is rotated off edge. 
            Default value is [-25.0]. *)
    | `Labeldistance of float
        (** Scaling factor for distance of head or tail label from node. 
            Default value is [1.0]. *)
    | `Labelfloat of bool
        (** If [true], lessen constraints on edge label placement. 
            Default value is [false]. *)
    | `Layer of string
        (** Overlay. *)
    | `Minlen of int
        (** Minimum rank distance between head an tail.  Default value is [1]. *)
    | `Samehead of string
        (** Tag for head node; edge heads with the same tag are merged onto the
	    same port. *)
    | `Sametail of string
        (** Tag for tail node; edge tails with the same tag are merged onto the
	    same port. *)
    | `Taillabel of string
        (** Sets the label attached to the tail arrow. *)
    | `Tailport of [ `N | `NE | `E | `SE | `S | `SW | `W | `NW ]
        (* TEMPORARY *)
    | `Tailurl of string
        (** Url attached to tail label if output format is ismap. *)
    | `Weight of int
        (** Sets the integer cost of stretching the edge.  Default value is
            [1]. *)
    ] 


  end

  module type INPUT = sig

    type graph
    type node
    type edge

    val iter_nodes: (node -> unit) -> graph -> unit
    val iter_edges: (edge -> unit) -> graph -> unit

    val graph_attributes: graph -> Attributes.graph list
    val default_node_attributes: graph -> Attributes.node list
    val default_edge_attributes: graph -> Attributes.edge list

    val node_name: node -> string
    val node_attributes: node -> Attributes.node list

    val edge_head: edge -> node
    val edge_tail: edge -> node
    val edge_attributes: edge -> Attributes.edge list

  end

  module Make (X: INPUT) : sig

    exception Error of string
    val handle_error: ('a -> 'b) -> 'a -> 'b

    (** [fprint_graph ppf graph] pretty prints the graph [graph] in
        the CGL language on the formatter [ppf]. 
     *)
    val fprint_graph: formatter -> X.graph -> unit

    (** [output_graph oc graph] pretty prints the graph [graph] in the dot
	language on the channel [oc].
     *)
    val output_graph: out_channel -> X.graph -> unit

    (** [run_graph output_mode f graph] runs the engine on the graph
        [graph].  The function [f] is applied with the input channel where
        the engine writes its output as argument.   
	This function must not close the channel.
        The format of the output is specified
        by the [output_mode] argument which may be one of the following
        [ `PostScript | `Mif | `HpGl | `Gif | `Imap | `Ismap | `Plain ]. 
     *)
    val run_graph:  
	[ `PostScript | `Mif | `HpGl | `Gif | `Imap | `Ismap | `Plain ] ->
	  (in_channel -> 'a) -> X.graph -> 'a

  end

end



(***************************************************************************)
(** {2 The neato engine} *)


module Neato : sig

  (** Several functions provided by this module run the external program
      {i neato}.  By default, this command is supposed to be in the default
      path and is invoked by {i neato}.  The function
      [set_command] allows to set an alternative path at run time.
   *)
  val set_command: string -> unit

  module Attributes : sig

  (** Attributes of graphs.  They include all common graph attributes and
      several specific ones.  All attributes described in the "Neato User's
      manual, April 10, 2002" are handled.
   *)
  type graph =
    [ CommonAttributes.graph
    | `Margin of float * float
        (** Sets the page margin (included in the page size).  Default value is
            [0.5, 0.5]. *)
    | `Start of int
        (** Seed for random number generator. *)
    | `Overlap of bool
	(** Default value is [true]. *)
    | `Spline of bool
	(** [true] makes edge splines if nodes don't overlap.
	    Default value is [false]. *)
    | `Sep of float
	(** Edge spline separation factor from nodes.  Default value 
	    is [0.0]. *)
    ] 

  (** Attributes of nodes.  They include all common node attributes and
      several specific ones.  All attributes described in the "Neato User's
      manual, April 10, 2002" are handled.
   *)
  type node =
    [ CommonAttributes.node
    | `Pos of float * float
        (** Initial coordinates of the node. *)
    ] 

  (** Attributes of edges.  They include all common edge attributes and
      several specific ones.  All attributes described in the "Neato User's
      manual, April 10, 2002" are handled.
   *)
  type edge =
    [ CommonAttributes.edge
    | `Id of string
        (** Optional value to distinguish multiple edges. *)
    | `Len of float
        (** Preferred length of edge.  Default value is [1.0]. *)
    | `Weight of float
        (** Strength of edge spring.  Default value is [1.0]. *)
    ] 

  end

  module type INPUT = sig

    type graph
    type node
    type edge

    val iter_nodes: (node -> unit) -> graph -> unit
    val iter_edges: (edge -> unit) -> graph -> unit

    val graph_attributes: graph -> Attributes.graph list
    val default_node_attributes: graph -> Attributes.node list
    val default_edge_attributes: graph -> Attributes.edge list

    val node_name: node -> string
    val node_attributes: node -> Attributes.node list

    val edge_head: edge -> node
    val edge_tail: edge -> node
    val edge_attributes: edge -> Attributes.edge list

  end

  module Make (X: INPUT) : sig

    exception Error of string

    val handle_error: ('a -> 'b) -> 'a -> 'b

    (** [fprint_graph ppf graph] pretty prints the graph [graph] in
        the CGL language on the formatter [ppf]. 
     *)
    val fprint_graph: formatter -> X.graph -> unit

    (** [output_graph oc graph] pretty prints the graph [graph] in the dot
	language on the channel [oc].
     *)
    val output_graph: out_channel -> X.graph -> unit

    (** [run_graph output_mode f graph] runs the engine on the graph
        [graph].  The function [f] is applied with the input channel where
        the engine writes its output as argument.   
	This function must not close the channel.
        The format of the output is specified
        by the [output_mode] argument which may be one of the following
        [ `PostScript | `Mif | `HpGl | `Gif | `Imap | `Ismap | `Plain ]. 
     *)
    val run_graph:  
	[ `PostScript | `Mif | `HpGl | `Gif | `Imap | `Ismap | `Plain ] ->
	  (in_channel -> 'a) -> X.graph -> 'a

  end

end
