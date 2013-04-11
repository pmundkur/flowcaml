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

(* $Id: transl_leveldecl.ml,v 1.5 2003/06/26 13:32:58 simonet Exp $ *)
(* Transl_level: translating level declarations *)

open Format
open Parsetree
open Path
open Level
open Types
open Transl_levelexpr



(* *************************************************************************
   Reporting errors
 *)

type error =
    Invalid_declaration of Level.t * Level.t


exception Error of Location.t * error

let error (loc, err) = raise (Error (loc, err))



let report_error ppf = function
    Invalid_declaration (level1, level2) ->
      fprintf ppf "This level declaration requires @[%a < %a@]"
	Level.fprint level1
	Level.fprint level2



(* *************************************************************************
   Translating and entering level declarations
 *)

let closure_lb env lvl0 lb =
  Level.Set.fold (fun lvl lb_closed ->
    match lvl with
      Tlvl_principal _ -> Level.Set.add lvl lb_closed
    | Tlvl_path p ->
	let lvd = try Env.find_level p env with Not_found -> assert false in
	Level.Set.union lvd.lvd_lb_closed lb_closed
  ) lb (Level.Set.singleton lvl0)

let closure_ub env lvl0 ub =
  Level.Set.fold (fun lvl ub_closed ->
    match lvl with
      Tlvl_principal _ -> Level.Set.add lvl ub_closed
    | Tlvl_path p ->
	let lvd = try Env.find_level p env with Not_found -> assert false in
	Level.Set.union lvd.lvd_ub_closed ub_closed
  ) ub (Level.Set.singleton lvl0)

let make_leveldecl env path lb ub =
  { lvd_lb = lb;
    lvd_ub = ub;
    lvd_lb_closed = closure_lb env (Tlvl_path path) lb;
    lvd_ub_closed = closure_ub env (Tlvl_path path) ub
  } 

let translate env id plvd =

  let lb = 
    List.fold_left (fun lb plvl ->
      Level.Set.add (transl_level env plvl) lb
    ) Level.Set.empty plvd.plvd_lb
  and ub =
    List.fold_left (fun ub plvl ->
      Level.Set.add (transl_level env plvl) ub
    ) Level.Set.empty plvd.plvd_ub
  in

  Set.iter (function level1 ->
    Set.iter (function level2 ->
      if not (Env.level_leq level1 level2) then
	error (plvd.plvd_loc, Invalid_declaration (level1, level2))
    ) ub
  ) lb;

  make_leveldecl env (Pident id) lb ub



let enter env name plvd =

  let id = Ident.create name in
  let lvd = translate env id plvd in
  let newenv = Env.add_level id lvd env in

  (id, lvd, newenv)



(* *************************************************************************
   Comparison
 *)

open Include_error

let compare env id decl1 decl2 =

  let error report =
    raise(Error[Level_declarations(id, decl1, decl2, report)])
  in

  let check level1 level2 =
    if not (Env.level_leq level1 level2) then
      error (Rlvd_missing (level1, level2))
  in

  let level1 = Tlvl_path (Pident id) in

  Level.Set.iter (fun level' -> check level' level1) decl2.lvd_lb;
  Level.Set.iter (fun level' -> check level1 level') decl2.lvd_ub

  
