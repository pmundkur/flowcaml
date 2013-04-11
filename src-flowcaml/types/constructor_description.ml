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

(* $Id: constructor_description.ml,v 1.3 2003/06/26 13:32:55 simonet Exp $ *)
(* Constructor_description *)

open Format
open Dalton_aux
open Types

let annot = ref false



include Solver.Scheme (struct

  type t = constructor_description

  let cset root =
    root.cstr_cset

  let copy cset' f root =
    { cstr_cset = cset';
      cstr_args = List.map f root.cstr_args;
      cstr_res = f root.cstr_res;
      cstr_level = Option.map f root.cstr_level;
      cstr_arity = root.cstr_arity
    } 

  let iter f root =
    List.iter (f Invariant) root.cstr_args;
    f Invariant root.cstr_res;
    f Invariant root.cstr_res; 
    (* Il faut appliquer deux fois f au type du résultat, c'est un hack pour
       que les variables ne sautent pas au pretty-print. *)
    Option.iter (f Invariant) root.cstr_level

  let iter2 f root1 root2 =
    List.iter2 (f Invariant) root1.cstr_args root2.cstr_args;
    f Invariant root1.cstr_res root2.cstr_res;
    Option.iter2 (f Invariant) root1.cstr_level root2.cstr_level

  let dumb_formatter = 
    make_formatter (fun _ _ _ -> ()) (fun _ -> ())

  let fprint ppf f_cset f_node root =
    f_node Invariant dumb_formatter root.cstr_res;
    if not !annot then begin
      let rec fprint_list ppf = function
	  [] -> assert false
	| typ :: [] -> f_node Invariant ppf typ
	| typ :: tl -> 
	    fprintf ppf "%a *@ %a" (f_node Invariant) typ fprint_list tl
      in
      match root.cstr_args with
	[] -> ()
      | list -> fprintf ppf " of %a" fprint_list list
    end 
    else begin
      match root.cstr_level with
	None -> ()
      | Some lvl -> fprintf ppf " # %a" (f_node Invariant) lvl
    end

end)



let _ =
  Subst.constructor_description_copy := copy



let identical cstr1 cstr2 =
  begin match cstr1.cstr_level, cstr2.cstr_level with
    None, None | Some _, Some _ -> true
  | None, Some _ | Some _, None -> false
  end
    &&
  equivalent cstr1 cstr2
