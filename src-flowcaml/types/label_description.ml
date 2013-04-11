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

(* $Id: label_description.ml,v 1.3 2003/06/26 13:32:56 simonet Exp $ *)
(* Label_description: *)

open Format
open Dalton_aux
open Asttypes
open Types



let annot = ref false

include Solver.Scheme (struct

  type t = label_description

  let cset root =
    root.lbl_cset

  let copy cset' f root =
    { lbl_cset = cset';
      lbl_arg = f root.lbl_arg;
      lbl_res = f root.lbl_res;
      lbl_level = Option.map f root.lbl_level;
      lbl_all = root.lbl_all;
      lbl_pos = root.lbl_pos;
      lbl_mut = root.lbl_mut
    } 

  let iter f root =
    (f Invariant) root.lbl_arg;
    (f Invariant) root.lbl_res; 
    (f Invariant) root.lbl_res; 
    (* Il faut appliquer deux fois f au type du résultat, c'est un hack pour
       que les variables ne sautent pas au pretty-print. *)
    Option.iter (f Invariant) root.lbl_level 


  let iter2 f root1 root2 =
    f Invariant root1.lbl_arg root2.lbl_arg;
    f Invariant root1.lbl_res root2.lbl_res;
    Option.iter2 (f Invariant) root1.lbl_level root2.lbl_level

  let fprint ppf f_cset f_node root =
    let dumb = Format.make_formatter (fun _ _ _ -> ()) (fun _ -> ()) in
    f_node Invariant dumb root.lbl_res;
    if not !annot then f_node Invariant ppf root.lbl_arg
    else begin
      match root.lbl_level with
	None -> ()
      | Some lvl -> Format.fprintf ppf " # %a" (f_node Invariant) lvl
    end

end)



let _ =
  Subst.label_description_copy := copy


let identical lbl1 lbl2 =
  begin match lbl1.lbl_level, lbl2.lbl_level with
    None, None | Some _, Some _ -> true
  | None, Some _ | Some _, None -> false
  end
    &&
  lbl1.lbl_mut = lbl2.lbl_mut
    &&
  equivalent lbl1 lbl2
