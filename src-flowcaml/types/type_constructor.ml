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

(* $Id: type_constructor.ml,v 1.2 2003/06/26 13:32:57 simonet Exp $ *)
(* Type_constr *)



type tc_desc =
    TCfunction
  | TCtuple of int
  | TCpath of Path.t



type t =
    { tc_desc: tc_desc;
      tc_kinds: Fkind.t list;
      tc_prop: (Dalton_aux.variance * bool) list;
      tc_fun: bool
    } 



let same c1 c2 =
  match c1.tc_desc, c2.tc_desc with
    TCfunction, TCfunction -> true
  | TCtuple i1, TCtuple i2 -> i1 = i2
  | TCpath p1, TCpath p2 -> Path.same p1 p2
  | _ -> false



let convert tc =
  List.map2 (fun k (v, a) ->
    { Dalton_aux.variance = v;
      Dalton_aux.kind = Fkind.daltonize k;
      Dalton_aux.ldestr = true;
      Dalton_aux.rdestr = a
    } 
  ) tc.tc_kinds tc.tc_prop
