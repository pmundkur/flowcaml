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

(* $Id: transl_exception.ml,v 1.3 2003/06/26 13:32:58 simonet Exp $ *)
(* Transl_exception: checking and translating exceptions declarations *)

(* TEMPORARY
   Il faut checker que les rebinds sont bien faits avec le bon type.
*)

open Dalton_aux
open Parsetree
open Path
open Types
open Transl_typeexpr



(***************************************************************************)

type error =
    Rebind_mismatch

exception Error of Location.t * error

let error (loc, err) = raise (Error (loc, err))



let report_error ppf = function
    Rebind_mismatch ->
      Format.fprintf ppf
        "The given type does not match that of the rebinded exception"



(***************************************************************************)

let translate env pexn =

  let exn =

    let opt_param, args = pexn.pexn_type in

    let cset = Solver.cset () in

    let kind_env, node_env, opt_param' =
      match opt_param with
	None -> (ref []), [], None
      | Some (name, _) -> 
	  let param = Solver.variable cset Katom in
	  (ref [name, Fkind.Ukind.level]), [name, param], (Some param)
    in

    List.iter
      (kind_type_expect ~exn:true false env kind_env [] Fkind.Ukind.typ)
      args;

    { exn_cset = cset;
      exn_repr = Exn_abstract;
      exn_param = opt_param';
      exn_args = List.map (transl_type cset env node_env) args;
      exn_arity = List.length args
    } 

  in

  ignore (Exception_declaration.solve exn);

  match pexn.pexn_manifest with

    Some (lid, _) ->

      let path, exn' = Env.lookup_exception pexn.pexn_loc lid env in
      let exn' = Exception_declaration.copy exn' in
      if not (Exception_declaration.equivalent exn' exn) then 
	error (pexn.pexn_loc, Rebind_mismatch);
      { exn' with
	exn_repr = Exn_manifest path
      } 

  | None -> exn



let enter env name pexn =

  let exn = translate env pexn in
  let id, newenv = Env.enter_exception name exn env in
  (id, exn, newenv)



(***************************************************************************)

let canonical_repr env exn =

  let rec canonical_repr_aux repr = function
      { exn_repr = Exn_abstract } -> repr
    | { exn_repr = (Exn_manifest path') as repr' } ->
	canonical_repr_aux repr' (Env.find_exception path' env)
  in

  canonical_repr_aux Exn_abstract exn




(***************************************************************************)

open Include_error

let compare env id decl1 decl2 =

  let error report =
    raise(Error[Exception_declarations(id, decl1, decl2, report)])
  in

  match canonical_repr env decl1, canonical_repr env decl2 with

    Exn_abstract, Exn_abstract ->
      if decl1.exn_arity <> decl2.exn_arity then error Rexn_arity_mismatch;
      if not (Exception_declaration.equivalent decl1 decl2) then
	error Rexn_type_mismatch

  | Exn_manifest _, Exn_abstract
  | Exn_abstract, Exn_manifest _ ->
      error Rexn_invalid_abstraction

  | Exn_manifest path1, Exn_manifest path2 ->
      if not (Path.same path1 path2) then error Rexn_repr_mismatch
