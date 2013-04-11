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

(* $Id: pattern.ml,v 1.2 2003/06/26 13:32:57 simonet Exp $ *)
(* Pattern: *)

open Asttypes
open Solver



type 'a pattern =
  { pat_desc: 'a desc;
    pat_loc: Location.t;
    pat_cset: cset;
    pat_context: 'a Pat_context.t;
    pat_typ: 'a;
    pat_level: 'a;
    pat_env: Env.t }

and 'a desc =
    Tpat_any
  | Tpat_var of Ident.t
  | Tpat_alias of 'a pattern * Ident.t
  | Tpat_constant of constant
  | Tpat_tuple of 'a pattern list
  | Tpat_construct of Constructor_description.t * 'a pattern option
  | Tpat_record of (Label_description.t * 'a pattern) list
  | Tpat_array of 'a pattern list
  | Tpat_or of 'a pattern * 'a pattern * Path.t option
