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

(* $Id: gen_source.ml,v 1.12 2003/06/27 13:09:57 simonet Exp $ *)
(* Gen_source: *)

open Printf
open Datastruct
open Location
open Asttypes
open Parsetree
open Stripinfo

exception Copy_error of int
exception Skip_error of int


let get_loc ic =
  pos_in ic


let copy_buffer = Buffer.create 128

let copy ic oc loc =
  let len = loc - pos_in ic in
  if len < 0 then raise (Copy_error loc);
  Buffer.add_channel copy_buffer ic len;
  Buffer.output_buffer oc copy_buffer;
  Buffer.clear copy_buffer


let skip ic loc =
  if loc < pos_in ic then raise (Skip_error loc);
  seek_in ic loc



(***************************************************************************)
(* Core language *)

let ptyp_paren ptyp =

  match ptyp.ptyp_desc with

    Ptyp_var _ | Ptyp_constr _ | Ptyp_paren _ -> true
  | Ptyp_arrow _ | Ptyp_tuple _ | Ptyp_arrow_abbrev _ -> false
  | Ptyp_bounds _ | Ptyp_row _ -> assert false


let rec core_type info ic oc ptyp =

  copy ic oc ptyp.ptyp_loc.loc_start;

  begin match ptyp.ptyp_desc with

    Ptyp_var name when name.[0] = '%' -> ()

  | Ptyp_var name -> 
      begin try
	let name' = StringMap.find name info.skel in
	fprintf oc "'%s" name';
	skip ic ptyp.ptyp_loc.loc_end
      with 
	Not_found -> ()
      end

  | Ptyp_arrow (arg, _, _, res, _) ->
      core_type info ic oc arg;
      fprintf oc " -> ";
      skip ic res.ptyp_loc.loc_start;
      core_type info ic oc res;

  | Ptyp_tuple list ->
      List.iter (function ptyp' ->
	core_type info ic oc ptyp'
      ) list

  | Ptyp_constr (_, []) -> ()

  | Ptyp_constr (lid, ((ptyp1 :: _) as list)) ->
      let list_info = 
	List.map (function ptyp -> ptyp, is_atomic info ptyp) list
      in

      let list_not_atomic =
	Standard.filter_map (function
	    _, true -> None
	  | ptyp, false -> Some ptyp
        ) list_info
      in

      begin match list_not_atomic with
	[] ->
	  skip ic ptyp.ptyp_loc.loc_end;
	  fprintf oc "%s" (Longident.to_string lid)

      |	[ptyp'] when ptyp_paren ptyp' ->
	  skip ic ptyp'.ptyp_loc.loc_start;
	  core_type info ic oc ptyp';
	  skip ic ptyp.ptyp_loc.loc_end;
	  fprintf oc " %s" (Longident.to_string lid)
	  
      |	_ ->
	  copy ic oc ptyp1.ptyp_loc.loc_start;
	  ignore (List.fold_left (fun first (ptyp', atomic) ->
	    if atomic then begin
	      skip ic ptyp'.ptyp_loc.loc_end;
	      first
	    end
	    else begin
	      if first then skip ic ptyp'.ptyp_loc.loc_start;
	      core_type info ic oc ptyp';
	      false
	    end
	  ) true list_info)
      end

  | Ptyp_bounds _
  | Ptyp_row _ -> assert false

  | Ptyp_arrow_abbrev ([], _, _, _) -> assert false

  | Ptyp_arrow_abbrev ((first_arg :: _) as args, _, _, res) ->
      copy ic oc first_arg.ptyp_loc.loc_start;
      List.iter (function arg ->
	skip ic arg.ptyp_loc.loc_start;
	core_type info ic oc arg;
	fprintf oc " -> ";
      ) args;
      skip ic res.ptyp_loc.loc_start;
      core_type info ic oc res

  | Ptyp_paren ptyp' ->

      core_type info ic oc ptyp'

  end;

  copy ic oc ptyp.ptyp_loc.loc_end



let core_scheme ic oc ptys =

  let info = find_scheme ptys.ptys_loc.loc_start in

  copy ic oc ptys.ptys_loc.loc_start;
  core_type info ic oc ptys.ptys_type;
  skip ic ptys.ptys_loc.loc_end



let constant ic oc = function
    Const_int _
  | Const_char _
  | Const_string _
  | Const_float _ -> ()
  | Const_charray (_, loc) -> 
      output_char oc '"';
      skip ic (loc.loc_start + 1);
      copy ic oc (loc.loc_end - 1);
      skip ic loc.loc_end;
      output_char oc '"'


let rec pattern ic oc ppat =

  copy ic oc ppat.ppat_loc.loc_start;

  begin match ppat.ppat_desc with

    Ppat_any
  | Ppat_var _ -> ()
  | Ppat_constant c -> constant ic oc c
  | Ppat_alias (ppat', _) -> pattern ic oc ppat'
  | Ppat_tuple list
  | Ppat_array list -> List.iter (pattern ic oc) list
  | Ppat_construct (lid, opt_ppat, _) -> Option.iter (pattern ic oc) opt_ppat
  | Ppat_record list -> List.iter (fun (_, ppat') -> pattern ic oc ppat') list
  | Ppat_or (ppat1, ppat2) -> pattern ic oc ppat1; pattern ic oc ppat2
  | Ppat_constraint (ppat, ptys) -> pattern ic oc ppat; core_scheme ic oc ptys
  | Ppat_paren ppat' -> pattern ic oc ppat'

  end;

  copy ic oc ppat.ppat_loc.loc_end



let try_pattern ident ic oc ptry =
  
  copy ic oc ptry.ptry_loc.loc_start;

  begin match ptry.ptry_desc with

    Ptry_any ->
      skip ic ptry.ptry_loc.loc_end;
      fprintf oc "%s when %s %s" ident Oast.catchable ident

  | Ptry_list list ->
      fprintf oc "(";
      List.iter (function ptryi ->
	Option.iter (pattern ic oc) ptryi.ptryi_arg;
	copy ic oc ptryi.ptryi_loc.loc_end
      ) list;
      fprintf oc ") as %s when %s %s" ident Oast.catchable ident

  end;

  copy ic oc ptry.ptry_loc.loc_end



let try_counter = ref (-1)


let pexp_paren pexp =

  match pexp.pexp_desc with
    Pexp_ident _ | Pexp_constant _ | Pexp_apply _ | Pexp_raise _ | Pexp_array _
  | Pexp_construct _ | Pexp_record _ | Pexp_field _ | Pexp_setfield _
  | Pexp_for _ | Pexp_while _ | Pexp_constraint _  | Pexp_assert _ 
  | Pexp_assertfalse | Pexp_paren _ -> true

  | Pexp_let _ | Pexp_function _ | Pexp_match _ | Pexp_try _ | Pexp_finally _
  | Pexp_tuple _ | Pexp_ifthenelse _ | Pexp_sequence _ -> false



let rec expr ic oc pexp =

  if not pexp.pexp_loc.loc_ghost then copy ic oc pexp.pexp_loc.loc_start;

  begin match pexp.pexp_desc with

    Pexp_ident _ -> ()
  | Pexp_constant c -> constant ic oc c

  | Pexp_let (_, list, pexp) ->
      patexp_list ic oc list;
      expr ic oc pexp

  | Pexp_function list ->
      patwhenexp_list ic oc list;

  | Pexp_apply (pexp, []) -> assert false
  | Pexp_apply (pexp, pexp1 :: list) ->
      if pexp.pexp_loc.loc_start < pexp1.pexp_loc.loc_start then begin
	expr ic oc pexp;
	expr ic oc pexp1;
      end
      else begin
	expr ic oc pexp1;
	expr ic oc pexp
      end;
      List.iter (expr ic oc) list

  | Pexp_match (pexp, list) ->
      expr ic oc pexp;
      patwhenexp_list ic oc list

  | Pexp_raise (lid, opt_pexp) ->
      Option.iter (expr ic oc) opt_pexp

  | Pexp_try (pexp', list) ->
      incr try_counter;
      let ident =
        "_exn" ^ (if !try_counter = 0 then "" else string_of_int !try_counter) 
      in
      expr ic oc pexp';
      List.iter (function ptry, pexp'', try_case ->
	match try_case with
	  Throw -> 
	    try_pattern ident ic oc ptry;
	    expr ic oc pexp''
	| Propagate loc ->
	    try_pattern ident ic oc ptry;
	    copy ic oc pexp''.pexp_loc.loc_start;

	    fprintf oc "%s begin fun () -> " Oast.propagate;
	    expr ic oc pexp'';
	    fprintf oc " end %s" ident;
	    skip ic loc.loc_end;
       ) list;
      decr try_counter

  | Pexp_finally (pexp1, pexp2) ->
      skip ic pexp1.pexp_loc.loc_start;
      fprintf oc "%s begin fun () -> " Oast.try_finally;
      expr ic oc pexp1;
      skip ic pexp2.pexp_loc.loc_start;
      fprintf oc " end begin fun () -> ";
      expr ic oc pexp2;
      fprintf oc " end";
      skip ic pexp.pexp_loc.loc_end

  | Pexp_tuple list
  | Pexp_array list ->
      List.iter (expr ic oc) list

  | Pexp_construct (lid, opt_pexp, _) ->
      Option.iter (expr ic oc) opt_pexp

  | Pexp_record (list, opt_pexp) ->
      List.iter (function _, pexp -> expr ic oc pexp) list;
      Option.iter (expr ic oc) opt_pexp

  | Pexp_field (pexp, _) ->
      expr ic oc pexp

  | Pexp_setfield (pexp1, _, pexp2) ->
      expr ic oc pexp1;
      expr ic oc pexp2

  | Pexp_ifthenelse (pexp1, pexp2, opt_pexp) ->
      expr ic oc pexp1;
      expr ic oc pexp2;
      Option.iter (expr ic oc) opt_pexp

  | Pexp_sequence (pexp1, pexp2)
  | Pexp_while (pexp1, pexp2) ->
      expr ic oc pexp1;
      expr ic oc pexp2

  | Pexp_for (_, pexp1, pexp2, _, pexp3) ->
      expr ic oc pexp1;
      expr ic oc pexp2;
      expr ic oc pexp3

  | Pexp_constraint (pexp, ptys) ->
      if pexp.pexp_loc.loc_end <= ptys.ptys_loc.loc_start then begin
	expr ic oc pexp;
	core_scheme ic oc ptys
      end else begin
	core_scheme ic oc ptys;
	expr ic oc pexp
      end

  | Pexp_assert pexp' ->
      expr ic oc pexp'

  | Pexp_assertfalse -> 
      ()

  | Pexp_paren pexp' ->
      expr ic oc pexp'

  end;

  if not pexp.pexp_loc.loc_ghost then copy ic oc pexp.pexp_loc.loc_end



and patexp_list ic oc list =
  List.iter (function ppat, pexp ->
    pattern ic oc ppat;
    expr ic oc pexp
  ) list



and patwhenexp_list ic oc list =
  List.iter (function ppat, opt_pexp, pexp ->
    pattern ic oc ppat;
    Option.iter (expr ic oc) opt_pexp;
    expr ic oc pexp
  ) list


  



(***************************************************************************)
(* Module language *)

(* Value descriptions *)

let value_description ic oc pval =

  copy ic oc pval.pval_type.ptys_loc.loc_start;
  core_scheme ic oc pval.pval_type



(* Type declarations *)

let type_declaration ic oc (id, ptype) =

  let info = find_typedecl ptype.ptype_loc.loc_start in

  copy ic oc ptype.ptype_loc.loc_start;

  fprintf oc " ";

  let params =
    Standard.filter_map (function name, _ ->
      if StringSet.mem name info.atomic then None else Some name
    ) ptype.ptype_params
  in

  begin match params with

    [] -> ()

  | [name] -> fprintf oc "'%s " name

  | name :: tl ->
      let rec list = function
	  [] -> ()
	| name :: tl -> fprintf oc ", '%s" name; list tl
      in
      fprintf oc "('%s" name;
      list tl;
      fprintf oc ") "

  end;

  skip ic ptype.ptype_loc'.loc_start;

  Option.iter (core_type info ic oc) ptype.ptype_manifest;

  let skip_annot = function
      None -> ()
    | Some (_, loc) ->
	copy ic oc loc.loc_start;
	skip ic loc.loc_end
  in

  begin match ptype.ptype_repr with

    Ptype_abstract -> ()

  | Ptype_variant (list, opt_level) ->
      List.iter (function _, ptyp_list ->
	List.iter (core_type info ic oc) ptyp_list
      ) list;
      skip_annot opt_level

  | Ptype_record (ptyp_list, opt_level) ->
      List.iter (function _, _, ptyp -> core_type info ic oc ptyp) ptyp_list;
      skip_annot opt_level

  end;

  copy ic oc ptype.ptype_loc.loc_end



(* Exception declarations *)

let exception_declaration_in_sig ic oc pexn =

  copy ic oc pexn.pexn_loc.loc_start;

  let opt_param, list = pexn.pexn_type in
  let info =
    match opt_param with
      None -> { atomic = StringSet.empty; skel = StringMap.empty }
    | Some (name, loc) ->
	copy ic oc loc.loc_start;
	skip ic loc.loc_end;
	{ atomic = StringSet.singleton name; skel = StringMap.empty }
  in
  List.iter (core_type info ic oc) list;

  begin match pexn.pexn_manifest with
    None -> ()
  | Some (_, loc) ->
      copy ic oc loc.loc_start;
      skip ic loc.loc_end
  end;

  copy ic oc pexn.pexn_loc.loc_end



let exception_declaration_in_str ic oc pexn = 

  copy ic oc pexn.pexn_loc.loc_start;

  begin match pexn.pexn_manifest with
    None ->
      let opt_param, list = pexn.pexn_type in
      let info =
	match opt_param with
	  None -> { atomic = StringSet.empty; skel = StringMap.empty }
	| Some (name, loc) ->
	    copy ic oc loc.loc_start;
	    skip ic loc.loc_end;
	    { atomic = StringSet.singleton name; skel = StringMap.empty }
      in
      List.iter (core_type info ic oc) list

  | Some (_, loc) ->
      skip ic loc.loc_start
  end;

  copy ic oc pexn.pexn_loc.loc_end



(* With constraints *)

let with_constraints ic oc list =
  ignore begin
    List.fold_left (fun kw pwth ->
      match pwth.pwth_desc with
	Pwith_type ptype ->
	  fprintf oc " %s " kw;
	  skip ic pwth.pwth_loc.loc_start;
	  type_declaration ic oc (pwth.pwth_ident, ptype);
	  copy ic oc pwth.pwth_loc.loc_end;
	  "and"
      | Pwith_level _ ->
	  kw
      | Pwith_module lid' ->
	  fprintf oc " %s " kw;
	  skip ic pwth.pwth_loc.loc_start;
	  copy ic oc pwth.pwth_loc.loc_end;
	  "and"
    ) "with" list
  end


(* Signatures *)

let rec signature ic oc psig_list =

  List.iter (signature_item ic oc) psig_list

and signature_item ic oc psig =

  copy ic oc psig.psig_loc.loc_start;

  begin match psig.psig_desc with

    Psig_value (name, pval) -> value_description ic oc pval

  | Psig_type list -> List.iter (type_declaration ic oc) list

  | Psig_level _ -> skip ic psig.psig_loc.loc_end

  | Psig_exception (name, pexn) -> exception_declaration_in_sig ic oc pexn

  | Psig_module (name, pmty) -> module_type ic oc pmty

  | Psig_modtype (name, modtype_decl) -> modtype_declaration ic oc modtype_decl

  | Psig_open _ -> ()

  | Psig_include pmty -> module_type ic oc pmty

  end;

  copy ic oc psig.psig_loc.loc_end



(* Module type declarations *)

and modtype_declaration ic oc = function
    Pmodtype_abstract -> ()
  | Pmodtype_manifest pmty -> module_type ic oc pmty



(* Module types *)

and module_type ic oc pmty =

  copy ic oc pmty.pmty_loc.loc_start;

  begin match pmty.pmty_desc with

    Pmty_ident _ -> ()

  | Pmty_signature psig -> signature ic oc psig

  | Pmty_functor (name, pmty1, _, _, pmty2, opt_loc) ->
      module_type ic oc pmty1;
      begin match opt_loc with
	None -> ()
      | Some loc ->
	  copy ic oc loc.loc_start;
	  fprintf oc "->";
	  skip ic loc.loc_end
      end;
      module_type ic oc pmty2

  | Pmty_with (pmty1, constraints) ->
      module_type ic oc pmty1;
      with_constraints ic oc constraints

  | Pmty_paren pmty' ->
      module_type ic oc pmty'

  end;

  copy ic oc pmty.pmty_loc.loc_end



(* Module expressions *)

and module_expr ic oc pmod =

  copy ic oc pmod.pmod_loc.loc_start;

  begin match pmod.pmod_desc with
    Pmod_ident _ -> ()
  | Pmod_structure pstr -> structure ic oc pstr
  | Pmod_functor (name, pmty, pmod') ->
      module_type ic oc pmty;
      module_expr ic oc pmod'
  | Pmod_apply (pmod1, pmod2) ->
      module_expr ic oc pmod1;
      module_expr ic oc pmod2
  | Pmod_constraint (pmod', pmty) -> 
      if pmod'.pmod_loc.loc_start < pmty.pmty_loc.loc_start then begin
	module_expr ic oc pmod';
	module_type ic oc pmty
      end
      else begin
	module_type ic oc pmty;
	module_expr ic oc pmod'
      end
  | Pmod_paren pmod' ->
      module_expr ic oc pmod'
  end;

  copy ic oc pmod.pmod_loc.loc_end



(* Structures *)

and structure ic oc pstr_list =

  List.iter (structure_item ic oc) pstr_list

and structure_item ic oc pstr =

  copy ic oc pstr.pstr_loc.loc_start;

  begin match pstr.pstr_desc with
    Pstr_eval pexp -> expr ic oc pexp
  | Pstr_value (_, list) -> patexp_list ic oc list
  | Pstr_primitive (name, pval) -> value_description ic oc pval
  | Pstr_type list -> List.iter (type_declaration ic oc) list
  | Pstr_level _ -> skip ic pstr.pstr_loc.loc_end
  | Pstr_exception (name, pexn) -> exception_declaration_in_str ic oc pexn
  | Pstr_module (name, pmod) -> module_expr ic oc pmod
  | Pstr_modtype (name, pmty) -> module_type ic oc pmty
  | Pstr_open _ -> ()
  | Pstr_include pmod -> module_expr ic oc pmod
  end;

  copy ic oc pstr.pstr_loc.loc_end




(***************************************************************************)
(* Entry points *)

let strip_file input_file output_chan stripper header_loc ast =

  let ic = open_in input_file in

  sort ();

  begin try
    copy ic output_chan header_loc.loc_start;
    skip ic header_loc.loc_end;
    stripper ic output_chan ast
  with
    Copy_error loc -> 
      Misc.fatal_error
	("striping " ^ input_file ^ ", copy to location: " ^ (string_of_int loc))
  | Skip_error loc -> 
      Misc.fatal_error
	("striping " ^ input_file ^ ", skip to location: " ^ (string_of_int loc))
  end;

  begin try
    while true do
      output_char output_chan (input_char ic)
    done;
  with End_of_file -> ()
  end;

  close_in ic



let interface input_file output_chan pint =
  Printf.fprintf output_chan "open %s\n" Oast.flowperv;
  strip_file input_file output_chan signature 
    pint.pint_header_loc
    pint.pint_signature

let implementation input_file output_chan pimp =
  Printf.fprintf output_chan "open %s\n" Oast.flowperv;
  strip_file input_file output_chan structure 
    pimp.pimp_header_loc
    pimp.pimp_structure
