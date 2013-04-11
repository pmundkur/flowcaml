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

(* $Id: datastruct.ml,v 1.2 2003/06/26 13:32:59 simonet Exp $ *)
(* Datastruct: Common data structures *)



module StringSet = struct

  include Set.Make (struct
    type t = string
    let compare = (Pervasives.compare : string -> string -> int)
  end)

  let of_list list =
    List.fold_left (fun set elt -> add elt set) empty list

end

module StringMap = struct

  include Map.Make (struct
    type t = string
    let compare = (Pervasives.compare : string -> string -> int)
  end)

  let of_list list =
    List.fold_left (fun map (key, elt) -> add key elt map) empty list

end

module StringHashtbl = Hashtbl.Make (struct
  type t = string
  let equal = ((=) : string -> string -> bool)
  let hash = (Hashtbl.hash : string -> int)
end)
