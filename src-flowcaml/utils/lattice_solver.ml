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

(* $Id: lattice_solver.ml,v 1.2 2003/06/26 13:33:00 simonet Exp $
   latticesolver.ml: A simple constraint solver.
 *)

(* The client must provide the following types and values. *)

module type INPUT = sig

  (* The type of the elements of the lattice. *)

  type t

  val bot: t
  val leq: t -> t -> bool
  val union: t -> t -> t

  type 'a term

  val iter: ('a -> unit) -> 'a term -> unit
  val eval: ('a -> t) -> 'a term -> t

end



module Make (X : INPUT) = struct

  type t = {
      mutable current: X.t;
      mutable successors: (t * t X.term) list;
    }	

  let create x =
    { current = x;
      successors = [];
    }	

  let bot = create X.bot

  let eval node =
    node.current

  let rec refresh x node =
    if not (X.leq x node.current) then begin
      node.current <- X.union x node.current;
      List.iter (function node', term ->
	refresh (X.eval eval term) node'
     ) node.successors
    end

  let leq term node =
    X.iter (function node' ->
      node'.successors <- (node, term) :: node'.successors
    ) term;
    refresh (X.eval eval term) node

end
