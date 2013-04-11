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

(* $Id: print_types.ml,v 1.7 2003/06/26 13:32:57 simonet Exp $ *)
(* Print_types: pretty-printing types. *)

open Dalton_aux
open Asttypes
open Label_description
open Type_declaration
open Types
open Format



let fprint_value_description id ppf vald =
  fprintf ppf "val %a : %a" 
    Ident.fprint id
    Value_description.fprint vald



let fprint_variant ppf (name, cstr) =
  fprintf ppf "@[<2>%s%a@]" 
    name
    Constructor_description.fprint cstr

let fprint_label ppf (name, lbl) =
  fprintf ppf "@[<2>%s%s :@ %a@];"
    (if lbl.lbl_mut = Mutable then "mutable " else "") 
    name
    Label_description.fprint lbl

let fprint_type_kind ppf = function
    Type_abstract -> ()
  | Type_variant variants ->
      let annot ppf =
	Constructor_description.annot := true;
	Constructor_description.fprint ppf (snd (List.hd variants));
	Constructor_description.annot := false
      in
      fprintf ppf " =@;<1 2>%a%t"
	(Misc.fprint_list (fun ppf -> fprintf ppf "@ | ") fprint_variant)
	variants
	annot
	

  | Type_record fields -> 
      let annot ppf =
	Label_description.annot := true;
	Label_description.fprint ppf (snd (List.hd fields));
	Label_description.annot := false
      in
      fprintf ppf " = {@ %a@;<1 -2>}%t"
	(Misc.fprint_list (fun ppf -> fprintf ppf "@ ") fprint_label) fields
	annot



let fprint_type_declaration id ppf decl =
  let fprint_type_parameters ppf =
    match List.combine decl.type_kinds decl.type_prop with
      [] -> fprintf ppf " "
    | list -> 
	let i = ref 0 in
	fprintf ppf " (@[<hv>%t)@] "
          (fun ppf -> List.iter (function k, (v, a) -> 
	    if !i > 0 then fprintf ppf ",@ ";
	    fprintf ppf "%s'%s:%a" 
	      (if a then "#" else Variance.to_string v)
	      (Misc.name_of_int !i) Fkind.fprint k;
	    incr i
          ) list);
  in

  fprintf ppf "@[<hv 2>type%s%t%a%a%a@]"
    (if decl.type_fun then " noneq" else "")
    fprint_type_parameters
    Ident.fprint id
    Type_declaration.fprint decl
    fprint_type_kind decl.type_repr



let fprint_level_declaration id ppf lvd =

  fprintf ppf "level %a" Ident.fprint id;
  let inter = Level.Set.inter lvd.lvd_lb lvd.lvd_ub in
  if Level.Set.is_empty inter then begin
    if not (Level.Set.is_empty lvd.lvd_lb) then 
      fprintf ppf " greater than @[%a@]" Level.Set.fprint lvd.lvd_lb;
    if not (Level.Set.is_empty lvd.lvd_ub) then
      fprintf ppf " less than @[%a@]" Level.Set.fprint lvd.lvd_ub
  end
  else
    fprintf ppf " = @[%a@]" Level.fprint (Level.Set.choose inter)



    
let fprint_exception_declaration id ppf exn =
  match exn.exn_repr with
    Exn_abstract ->
      fprintf ppf "exception %a%a"
	Ident.fprint id Exception_declaration.fprint exn
  | Exn_manifest path ->
      fprintf ppf "exception %a%a = %a"
	Ident.fprint id  
	Exception_declaration.fprint exn
	Path.fprint path



let rec fprint_signature ppf = function
    [] -> ()
  | item :: [] -> fprint_signature_item ppf item
  | item :: tail ->
      fprintf ppf "%a@ %a" 
	fprint_signature_item item
	fprint_signature tail

and fprint_signature_item ppf = function
    Tsig_value (id, value_description) ->
      fprintf ppf "@[<2>%a@]" 
	(fprint_value_description id) value_description
  | Tsig_type (id, type_declaration) ->
      fprintf ppf "@[<2>%a@]" 
	(fprint_type_declaration id) type_declaration
  | Tsig_level (id, level_declaration) ->
      fprintf ppf "@[<2>%a@]"
	(fprint_level_declaration id) level_declaration
  | Tsig_exception (id, exception_declaration) ->
      fprintf ppf "@[<2>%a@]"
	(fprint_exception_declaration id) exception_declaration
  | Tsig_module (id, module_type) ->
      fprintf ppf "@[<2>module %a :@ %a@]"
	Ident.fprint id fprint_module_type module_type
  | Tsig_modtype (id, mtydecl) ->
      fprintf ppf "@[<2>%a@]" 
	(fprint_modtype_declaration id) mtydecl
     
  
and fprint_module_type ppf = function
    Tmty_ident path -> Path.fprint ppf path
  | Tmty_signature sg -> 
      fprintf ppf "@[<hv 2>sig@ %a@;<1 -2>end@]" fprint_signature sg   
  | Tmty_functor (id, mty1, pci, pcf, mty2) ->
      let fprint_arrow ppf =
	match Level.Set.is_empty pci, Level.Set.is_empty pcf with
	  true, true -> 
	    fprintf ppf "->"
	| false, true ->
	    fprintf ppf "-{@[%a@] |}->" Level.Set.fprint pci
	| true, false ->
	    fprintf ppf "-{| @[%a@]}->" Level.Set.fprint pcf
	| false, false ->
	    fprintf ppf "-{@[%a@] | @[%a@]}->"
	      Level.Set.fprint pci
	      Level.Set.fprint pcf
      in
      fprintf ppf "@[<2>functor@ (%a : %a) %t@ %a@]"
	Ident.fprint id
	fprint_module_type mty1
	fprint_arrow
	fprint_module_type mty2


and fprint_modtype_declaration id ppf = function
    Tmodtype_abstract -> 
      fprintf ppf "@[<2>module type %a@]" Ident.fprint id
  | Tmodtype_manifest mty ->
      fprintf ppf "@[<2>module type %a =@ %a@]" 
      Ident.fprint id  fprint_module_type mty

