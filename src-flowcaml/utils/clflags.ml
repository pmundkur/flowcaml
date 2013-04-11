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

(* $Id: clflags.ml,v 1.5 2003/06/26 13:32:59 simonet Exp $ *)
(* Clflags: command line parameters *)

let print_types = ref false                  (* -i *)
let preprocessor = ref (None : string option)
and include_dirs = ref ([] : string list)
and nopervasives = ref false                 (* -nopervasives *)
and nostdlib = ref false                     (* -nostdlib *)
and debug = ref false                        (* -debug *)
and verbose = ref false                      (* -verbose *)
and graph = ref false                        (* -graph *)
and display = ref Config.default_display     (* -display *)
and dumped_output = ref false                (* -dump *)
and geometry = ref ""                        (* -geometry *)
and debug_stats = ref false                  (* -stat *)
and font = ref ""                            (* -font *)

type output_mode =
    Ofile
  | Oquiet
  | Opp
(*
  | Ocomp_byte
  | Ocomp_asm
*)

let output_mode = ref Ofile

let std_include_dir () =
  if !nostdlib then [] else [Config.standard_library]
