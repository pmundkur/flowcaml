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

(* $Id: longident.ml,v 1.2 2003/06/26 13:32:49 simonet Exp $ *)
(* Longident: *)



type t =
    Lident of string
  | Ldot of t * string
  | Lapply of t * t

open Format

let lident = function
    Lident name -> name
  | _ -> raise Not_found


let rec flat accu = function
    Lident s -> s :: accu
  | Ldot(lid, s) -> flat (s :: accu) lid
  | Lapply(l1, l2) -> Misc.fatal_error "Longident.flat"

let flatten lid = flat [] lid

let rec fprint ppf = function
    Lident name -> fprintf ppf "%s" name
  | Ldot (lid, name) -> fprintf ppf "%a.%s" fprint lid name
  | Lapply (lid1, lid2) ->
      fprintf ppf "%a(%a)" fprint lid1 fprint lid2

let rec to_string = function
    Lident name -> name
  | Ldot (lid, name) -> 
      (to_string lid) ^ "." ^ name
  | Lapply (lid1, lid2) ->
      (to_string lid1) ^ "(" ^ (to_string lid2) ^ ")"
