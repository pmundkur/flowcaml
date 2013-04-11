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

(* $Id: value_description.ml,v 1.3 2003/06/26 13:32:57 simonet Exp $ *)
(* Value_description: *)

open Format
open Dalton_aux
open Types



(* TEMPORARY C'est un peu un hack de mettre ça ici. *)
open Type_constructor
open Solver


let _ =
  Solver.set_expand_manifest begin function t ->
    match t.constr.tc_desc with
      TCfunction | TCtuple _ -> None
    | TCpath path ->
	try
	  let decl = Env.find_type path !Env.current_env in
	  match decl.type_manifest with
	    None -> None
	  | Some typ_to ->
	      let typ_from =
		Solver.typ decl.type_cset
		  { constr = { tc_desc = TCpath path;
			       tc_kinds = decl.type_kinds;
			       tc_prop = decl.type_prop;
			       tc_fun = decl.type_fun
			     } ;
		    sons = decl.type_params
		  }	
	      in
	      Some (typ_from, typ_to)
	with 
	  Not_found -> None
  end



include Solver.Scheme (struct

  type t = value_description

  let cset root =
    root.val_cset

  let copy cset' f root =
    { val_cset = cset';
      val_context = Expr_context.map f root.val_context;
      val_typ = f root.val_typ
    } 

  let iter f root =
    Expr_context.iter (f Contravariant) root.val_context;
    f Covariant root.val_typ

  let iter2 f root1 root2 =
    assert (root1.val_context = Expr_context.empty
	      && root2.val_context = Expr_context.empty);
    f Covariant root1.val_typ root2.val_typ



  let fprint ppf f_cset f_node root =
    if root.val_context = Expr_context.empty then
      fprintf ppf "@[<v>@[%a@]%a@]" 
	(f_node Covariant) root.val_typ f_cset root.val_cset
    else
      fprintf ppf "@[<v>[%a] |- %a@ %a@]"
	(Expr_context.fprint (f_node Contravariant)) root.val_context
	(f_node Covariant) root.val_typ
	f_cset root.val_cset

end)



let _ =
  Subst.value_description_copy := copy
