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

(* $Id: stripinfo.ml,v 1.3 2003/06/26 13:32:54 simonet Exp $ *)
(* Stripinfo: *)

open Datastruct
open Location
open Parsetree



(***************************************************************************)
(* Memoizing informations *)

(* Informations about type declarations *)

type info =
    { atomic: StringSet.t;
      skel: string StringMap.t
    }

let typedecl_register : (int * info) list ref = ref []

let store_typedecl loc atomic_variables =
  let info = 
    { atomic = StringSet.of_list atomic_variables;
      skel = StringMap.empty
    }
  in
  typedecl_register := (loc.loc_end, info) :: !typedecl_register



(* Informations about schemes *)

let scheme_register : (int * info) list ref = ref []

let store_scheme loc atomic_variables skel =
  let info =
    { atomic = StringSet.of_list atomic_variables;
      skel = StringMap.of_list skel
    } 
  in
  scheme_register := (loc.loc_end, info) :: !scheme_register



(***************************************************************************)
(* Reading informations *)

let sort () =
  let cmp (i1, _) (i2, _) = Pervasives.compare i1 i2 in
  typedecl_register := List.sort cmp !typedecl_register;
  scheme_register := List.sort cmp !scheme_register

let rec find register loc =
  match !register with
    [] -> raise Not_found
  | (loc', info) :: tl ->
      if loc <= loc' then info
      else begin
	register := tl;
	find register loc
      end

let find_typedecl loc = find typedecl_register loc
let find_scheme loc = find scheme_register loc

let rec is_atomic info ptyp =

    match ptyp.ptyp_desc with

      Ptyp_var name ->
	StringSet.mem name info.atomic

    | Ptyp_bounds _
    | Ptyp_row _ -> true

    | Ptyp_arrow _
    | Ptyp_tuple _
    | Ptyp_constr _
    | Ptyp_arrow_abbrev _ -> false

    | Ptyp_paren ptyp' -> is_atomic info ptyp'



let reset () =
  typedecl_register := [];
  scheme_register := []
