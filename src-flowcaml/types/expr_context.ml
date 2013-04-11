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

(* $Id: expr_context.ml,v 1.2 2003/06/26 13:32:56 simonet Exp $ *)
(* Expr_context: Context of value and expression schemes. *)



type 'a t = 'a Ident.tbl

let empty = Ident.empty
let map = Ident.map
let iter = Ident.iter
let fold = Ident.fold
let find_name = Ident.find_name
let find_same = Ident.find_same
let remove = Ident.remove
let add = Ident.add
let singleton id data = Ident.add id data empty


let rec fprint f ppf env = 
  ignore (fold (fun id typ b ->
    if not b then Format.fprintf ppf "; ";
    Format.fprintf ppf "%a: %a" Ident.fprint id f typ;
    false
  ) env true)
