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

(* $Id: subst.ml,v 1.5 2003/06/26 13:32:57 simonet Exp $ *)
(* Subst *)

open Path
open Type_constructor
open Level
open Types
open Solver



(***************************************************************************)
(** {2 Substitutions} *)

type t = 
  { types: (Ident.t, Path.t) Tbl.t;
    levels: (Ident.t, Path.t) Tbl.t;
    exceptions: (Ident.t, Path.t) Tbl.t;
    modules: (Ident.t, Path.t) Tbl.t;
    modtypes: (Ident.t, module_type) Tbl.t }

let identity =
  { types = Tbl.empty; 
    levels = Tbl.empty;
    exceptions = Tbl.empty;
    modules = Tbl.empty; 
    modtypes = Tbl.empty }

let add_type id p s =
  { s with types = Tbl.add id p s.types }

let add_level id p s =
  { s with levels = Tbl.add id p s.levels }

let add_exception id p s =
  { s with exceptions = Tbl.add id p s.exceptions }

let add_module id p s =
  { s with modules = Tbl.add id p s.modules }

let add_modtype id ty s =
  { s with modtypes = Tbl.add id ty s.modtypes }



(***************************************************************************)
(** {2 Forward declarations} *)

type m = skeleton typ -> bool

let value_description_copy : 
    (?subst:subst -> ?expand:m -> value_description -> value_description) ref
    = ref (fun ?subst ?expand _ -> assert false)
let constructor_description_copy : 
    (?subst:subst -> ?expand:m -> constructor_description -> 
      constructor_description) ref
    = ref (fun ?subst ?expand _ -> assert false)
let label_description_copy : 
    (?subst:subst -> ?expand:m -> label_description -> label_description) ref
    = ref (fun ?subst ?expand _ -> assert false)
let type_declaration_copy : 
    (?subst:subst -> ?expand:m -> type_declaration -> 
      type_declaration) ref
    = ref (fun ?subst ?expand _ -> assert false)
let exception_declaration_copy : 
    (?subst:subst -> ?expand:m -> exception_declaration -> 
      exception_declaration) ref
    = ref (fun ?subst ?expand _ -> assert false)
  


(***************************************************************************)
(** {2 Applying substitutions} *)

let rec module_path s = function
    Pident id as p ->
      begin try Tbl.find id s.modules with Not_found -> p end
  | Pdot(p, n, pos) ->
      Pdot(module_path s p, n, pos)
  | Papply(p1, p2) ->
      Papply(module_path s p1, module_path s p2)

let type_path s = function
    Pident id as p ->
      begin try Tbl.find id s.types with Not_found -> p end
  | Pdot(p, n, pos) ->
      Pdot(module_path s p, n, pos)
  | Papply(p1, p2) ->
      Misc.fatal_error "Subst.type_path"

let level_path s = function
    Pident id as p ->
      begin try Tbl.find id s.levels with Not_found -> p end
  | Pdot(p, n, pos) ->
      Pdot(module_path s p, n, pos)
  | Papply(p1, p2) ->
      Misc.fatal_error "Subst.level_path"

let exception_path s = function
    Pident id as p ->
      begin try Tbl.find id s.exceptions with Not_found -> p end
  | Pdot(p, n, pos) ->
      Pdot(module_path s p, n, pos)
  | Papply(p1, p2) ->
      Misc.fatal_error "Subst.exception_path"



let level s = function
    (Tlvl_principal _) as l -> l
  | Tlvl_path p -> 
      Tlvl_path (level_path s p)

let level_set s set =
  Level.Set.map (level s) set



let daltonize s =
  let type_s t =
    { constr =
        { t.constr with 
	  tc_desc =
	    match t.constr.tc_desc with
	      (TCfunction | TCtuple _) as d -> d
	    | TCpath p -> TCpath (type_path s p)
	} ;
      sons = t.sons
    } 
  in

  { lb = level_set s;
    ub = level_set s;
    typ = type_s;
    label = exception_path s
  }



let value_description s vald =
  !value_description_copy ~subst:(daltonize s) vald

let constructor_description s cstr =
  !constructor_description_copy ~subst:(daltonize s) cstr

let label_description s lbl =
  !label_description_copy ~subst:(daltonize s) lbl

let type_declaration s decl =
  { (!type_declaration_copy ~subst:(daltonize s) decl) with
    type_repr =
      begin match decl.type_repr with
        Type_abstract -> Type_abstract
      | Type_variant cstrs ->
          Type_variant (List.map (fun (n, desc) -> 
	    n, constructor_description s desc
          ) cstrs)
      | Type_record (lbls) ->
          Type_record (List.map (fun (n, desc) -> 
	    n, label_description s desc
          ) lbls)
      end;
  } 

let level_declaration s lvd =
  { lvd_lb = level_set s lvd.lvd_lb;
    lvd_ub = level_set s lvd.lvd_ub;
    lvd_lb_closed = level_set s lvd.lvd_lb_closed;
    lvd_ub_closed = level_set s lvd.lvd_ub_closed
  } 



let exception_declaration s exn =
  { (!exception_declaration_copy ~subst:(daltonize s) exn) with
    exn_repr =
    begin match exn.exn_repr with
      Exn_manifest p -> Exn_manifest (exception_path s p)
    | Exn_abstract -> Exn_abstract
    end
  } 



let rec rename_bound_idents s idents = function
    [] -> (List.rev idents, s)
  | Tsig_type(id, d) :: sg ->
      let id' = Ident.rename id in
      rename_bound_idents (add_type id (Pident id') s) (id' :: idents) sg
  | Tsig_level(id, d) :: sg ->
      let id' = Ident.rename id in
      rename_bound_idents (add_level id (Pident id') s) (id' :: idents) sg
  | Tsig_module(id, mty) :: sg ->
      let id' = Ident.rename id in
      rename_bound_idents (add_module id (Pident id') s) (id' :: idents) sg
  | Tsig_modtype(id, d) :: sg ->
      let id' = Ident.rename id in
      rename_bound_idents (add_modtype id (Tmty_ident(Pident id')) s)
                          (id' :: idents) sg
  | (Tsig_value(id, _) | Tsig_exception(id, _)) :: sg ->
      let id' = Ident.rename id in
      rename_bound_idents s (id' :: idents) sg



let rec modtype s = function
    Tmty_ident p as mty ->
      begin match p with
        Pident id ->
          begin try Tbl.find id s.modtypes with Not_found -> mty end
      | Pdot(p, n, pos) ->
          Tmty_ident(Pdot(module_path s p, n, pos))
      | Papply(p1, p2) ->
          Misc.fatal_error "Subst.modtype"
      end
  | Tmty_signature sg ->
      Tmty_signature(signature s sg)
  | Tmty_functor(id, arg, pci, pcf, res) ->
      let id' = Ident.rename id in
      Tmty_functor(id', modtype s arg,
		        level_set s pci, level_set s pcf,
                        modtype (add_module id (Pident id') s) res)



and signature s sg =
  (* Components of signature may be mutually recursive (e.g. type declarations
     or class and type declarations), so first build global renaming
     substitution... *)
  let (new_idents, s') = rename_bound_idents s [] sg in
  (* ... then apply it to each signature component in turn *)
  List.map2 (signature_component s') sg new_idents



and signature_component s comp newid =
  match comp with
    Tsig_value(id, d) ->
      Tsig_value(newid, value_description s d)
  | Tsig_type(id, d) ->
      Tsig_type(newid, type_declaration s d)
  | Tsig_level(id, d) ->
      Tsig_level(newid, level_declaration s d)
  | Tsig_exception(id, d) ->
      Tsig_exception(newid, exception_declaration s d)
  | Tsig_module(id, mty) ->
      Tsig_module(newid, modtype s mty)
  | Tsig_modtype(id, d) ->
      Tsig_modtype(newid, modtype_declaration s d)


and modtype_declaration s = function
    Tmodtype_abstract -> Tmodtype_abstract
  | Tmodtype_manifest mty -> Tmodtype_manifest(modtype s mty)
