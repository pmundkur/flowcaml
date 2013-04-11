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

(* $Id: doc_parsetree.ml,v 1.4 2003/06/26 13:32:49 simonet Exp $ *)

type ftext = ftext_item list

and ftext_item =
    Directive of string
  | String of string
  | Block of string * ftext

type content =
    Terminate
  | Ftext of ftext

type comment =
    { cmt_start: int;
      cmt_end: int;
      cmt_content: content
    }
