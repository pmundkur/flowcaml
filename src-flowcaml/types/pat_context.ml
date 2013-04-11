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

(* $Id: pat_context.ml,v 1.3 2003/06/26 13:32:57 simonet Exp $ *)
(* Pat_context *)

open Datastruct

type 'a t = (Ident.t * 'a) StringMap.t
let empty = StringMap.empty
let iter f m = StringMap.iter (fun _ (_, x) -> f x) m
let iter_id f m = StringMap.iter (fun _ (id, _) -> f id) m
let map f m = StringMap.map (function id, x -> id, f x) m
let fold = StringMap.fold
let mem = StringMap.mem
let find = StringMap.find
let remove = StringMap.remove
let add = StringMap.add
let singleton id x = StringMap.add id x empty

let rec fprint f ppf env = 
  ignore (fold (fun name (id, x) b ->
    if not b then Format.fprintf ppf "; ";
    Format.fprintf ppf "%a: %a" Ident.fprint id f x;
    false
  ) env true)
