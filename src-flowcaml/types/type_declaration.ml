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

(* $Id: type_declaration.ml,v 1.3 2003/06/26 13:32:57 simonet Exp $ *)
(* Type_declaration *)

open Format
open Dalton_aux
open Types



include Solver.Scheme (struct

  type t = type_declaration

  let cset root = root.type_cset

  let copy cset' f root =
    { root with
      type_cset = cset';
      type_params = List.map f root.type_params;
      type_manifest = Option.map f root.type_manifest;
    } 

  let iter f root =
    List.iter (f Invariant) root.type_params;
    Option.iter (f Invariant) root.type_manifest

  let iter2 f root1 root2 =
    List.iter2 (f Invariant) root1.type_params root2.type_params;
    Option.iter2 (f Invariant) root1.type_manifest root2.type_manifest

  let fprint ppf f_cset f_node root =
    let dumb = Format.make_formatter (fun _ _ _ -> ()) (fun _ -> ()) in
    List.iter (f_node Invariant dumb) root.type_params;
    match root.type_manifest with
      None -> ()
    | Some typ -> fprintf ppf " =@ %a" (f_node Invariant) typ

end)



let _ =
  Subst.type_declaration_copy := copy
