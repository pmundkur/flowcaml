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

(* $Id: exception_declaration.ml,v 1.2 2003/06/26 13:32:55 simonet Exp $ *)
(* Exception_declaration: *)

open Format
open Dalton_aux
open Types



include Solver.Scheme (struct

  type t = exception_declaration

  let cset root = root.exn_cset

  let copy cset' f root =
    { exn_cset = cset';
      exn_repr = root.exn_repr;
      exn_param = Option.map f root.exn_param;
      exn_args = List.map f root.exn_args;
      exn_arity = root.exn_arity
    } 

  let iter f root =
    Option.iter (f Invariant) root.exn_param;
    List.iter (f Invariant) root.exn_args

  let iter2 f root1 root2 =
    begin match root1.exn_param, root2.exn_param with
      Some t1, Some t2 -> f Invariant t1 t2
    | _ -> ()
    end;
    List.iter2 (f Invariant) root1.exn_args root2.exn_args

  let fprint ppf f_cset f_node root =
    let rec fprint_list ppf = function
	[] -> assert false
      | typ :: [] -> f_node Invariant ppf typ
      | typ :: tl -> 
	  Format.fprintf ppf "%a *@ %a" (f_node Invariant) typ fprint_list tl
    in
    let fprint_option ppf = function
	None -> ()
      |	Some t -> fprintf ppf " : %a" (f_node Invariant) t
    in
    match root.exn_args with
      [] -> ()
    | list -> Format.fprintf ppf "%a of %a" 
	  fprint_option root.exn_param
	  fprint_list list

end)

let _ =
  Subst.exception_declaration_copy := copy
