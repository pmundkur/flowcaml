(**************************************************************************)
(*                                                                        *)
(*                                  Dalton                                *)
(*                      an efficient implementation of                    *)
(*                 type inference with structural subtyping               *)
(*                                                                        *)
(*          Vincent Simonet, Projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*  Copyright 2002, 2003 Institut National de Recherche en Informatique   *)
(*  et en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with the   *)
(*  special exception on linking described in file LICENSE.               *)
(*                                                                        *)
(*  Author contact: Vincent.Simonet@inria.fr                              *)
(*  Software page: http://cristal.inria.fr/~simonet/soft/dalton/          *)
(*                                                                        *)
(**************************************************************************)

(* $Id: dalton_debug.ml,v 1.4 2003/06/26 13:32:47 simonet Exp $ *)



let skip_terminal = ref false

let terminal = ref 10



(***************************************************************************)
(** {2 Statistics} *)

module Stat = struct

  type t = 
      { mutable node: int;
	mutable node_variable: int;
	mutable node_ext: int;
	mutable node_copy: int;
	mutable node_expand: int;
	mutable non_terminal: int;
	mutable expanded: int;
	mutable cycle: int;
	mutable fork: int;
	mutable minimization: int;
	mutable gc: int;
	mutable hash_consing: int
      }

  let create () =
    { node = 0;
      node_variable = 0;
      node_ext = 0;
      node_copy = 0;
      node_expand = 0;
      non_terminal = 0;
      expanded = 0;
      cycle = 0;
      fork = 0;
      minimization = 0;
      gc = 0;
      hash_consing = 0
    }

  let major = create ()
  let minor = create ()

  let iter f =
    f major; f minor

  let reset s = 
    s.node <- 0;
    s.node_variable <- 0;
    s.node_ext <- 0;
    s.node_copy <- 0;
    s.node_expand <- 0;
    s.non_terminal <- 0;
    s.expanded <- 0;
    s.cycle <- 0;
    s.fork <- 0;
    s.minimization <- 0;
    s.gc <- 0;
    s.hash_consing <- 0

  let pct x y =
    if y = 0 then y else 100 * x / y

  let output oc s =
    Printf.fprintf oc "* Total number of nodes   : %6i\n" 
      s.node;
    Printf.fprintf oc "  created as variables    : %6i (%3i%%)\n" 
      s.node_variable (pct s.node_variable s.node);
    Printf.fprintf oc "  created by client       : %6i (%3i%%)\n" 
      s.node_ext (pct s.node_ext s.node);
    Printf.fprintf oc "  created by copy         : %6i (%3i%%)\n"
      s.node_copy (pct s.node_copy s.node);
    Printf.fprintf oc "  created by expansion    : %6i (%3i%%)\n\n"
      s.node_expand (pct s.node_expand s.node);
    Printf.fprintf oc "* Non-terminal nodes      : %6i\n" 
      s.non_terminal;
    Printf.fprintf oc "  expanded ones           : %6i (%3i%%)\n\n"
      s.expanded (pct s.expanded s.non_terminal);
    Printf.fprintf oc "* Simplifications\n";
    Printf.fprintf oc "  Cycles                  : %6i\n"
      s.cycle;
    Printf.fprintf oc "  Chains                  : %6i\n"
      s.fork;
    Printf.fprintf oc "  Garbage collection      : %6i\n"
      s.gc;
    Printf.fprintf oc "  Minimization            : %6i\n"
      s.minimization;
    Printf.fprintf oc "  Hash-consing            : %6i\n"
      s.hash_consing

      
  let reset_major () = iter reset
  let reset_minor () = reset minor
  let output_major oc = output oc major
  let output_minor oc = output oc minor
      

  let node () =
    iter (fun t -> t.node <- t.node + 1);
    true

  let node_variable () =
    iter (fun t -> t.node_variable <- t.node_variable + 1);
    true

  let node_ext () =
    iter (fun t -> t.node_ext <- t.node_ext + 1);
    true

  let node_copy () =
    iter (fun t -> t.node_copy <- t.node_copy + 1);
    true

  let node_expand () =
    iter (fun t -> t.node_expand <- t.node_expand + 1);
    true

  let non_terminal () =
    iter (fun t -> t.non_terminal <- t.non_terminal + 1);
    true

  let expanded () =
    iter (fun t -> t.expanded <- t.expanded + 1);
    true

  let cycle () =
    iter (fun t -> t.cycle <- t.cycle + 1);
    true

  let fork () =
    iter (fun t -> t.fork <- t.fork + 1);
    true

  let gc () =
    iter (fun t -> t.gc <- t.gc + 1);
    true

  let minimization () =
    iter (fun t -> t.minimization <- t.minimization + 1);
    true

  let hash_consing () =
    iter (fun t -> t.hash_consing <- t.hash_consing + 1);
    true

end
