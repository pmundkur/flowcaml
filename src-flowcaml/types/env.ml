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

(* $Id: env.ml,v 1.6 2003/06/26 13:32:55 simonet Exp $ *)
(* Env *)

open Longident
open Path
open Type_constructor
open Level
open Types
open Format
open Solver



(***************************************************************************)
(** {2 Reporting errors} *)

type lookup_error =
    Unbound_value of Longident.t
  | Unbound_constr of Longident.t
  | Unbound_label of Longident.t
  | Unbound_type of Longident.t
  | Unbound_level of Longident.t
  | Unbound_type_or_level of Longident.t
  | Unbound_exception of Longident.t
  | Unbound_module of Longident.t
  | Unbound_modtype of Longident.t

exception LookupError of Location.t * lookup_error

let report_lookup_error ppf = function

  | Unbound_value lid ->
      fprintf ppf "Unbound value %a" Longident.fprint lid
  | Unbound_constr lid ->
      fprintf ppf "Unbound constructor %a" Longident.fprint lid
  | Unbound_label lid ->
      fprintf ppf "Unbound record field label %a" Longident.fprint lid
  | Unbound_type lid ->
      fprintf ppf "Unbound type %a" Longident.fprint lid
  | Unbound_level lid ->
      fprintf ppf "Unbound level %a" Longident.fprint lid
  | Unbound_type_or_level lid ->
      fprintf ppf "Unbound type or level %a" Longident.fprint lid
  | Unbound_exception lid ->
      fprintf ppf "Unbound exception %a" Longident.fprint lid
  | Unbound_module lid ->
      fprintf ppf "Unbound module %a" Longident.fprint lid
  | Unbound_modtype lid ->
      fprintf ppf "Unbound module type %a" Longident.fprint lid



type error =
    Not_an_interface of string
  | Corrupted_interface of string
  | Illegal_renaming of string * string
  | Inconsistent_import of string * string * string

exception Error of error

let report_error ppf = function
  | Not_an_interface filename -> Format.fprintf ppf
      "%s@ is not a compiled interface" filename
  | Corrupted_interface filename -> Format.fprintf ppf
      "Corrupted compiled interface@ %s" filename
  | Illegal_renaming(modname, filename) -> Format.fprintf ppf
      "Wrong file naming: %s@ contains the compiled interface for@ %s"
      filename modname
  | Inconsistent_import(name, source1, source2) -> Format.fprintf ppf
      "@[<hov>The compiled interfaces %s@ and %s@ \
              make inconsistent assumptions over interface %s@]"
      source1 source2  name



(***************************************************************************)
(** {2 Datatype definitions} *)

type constructor_or_exception =
    Constructor of constructor_description
  | Exception of exception_declaration

type t =
    { values: (Path.t * value_description option) Ident.tbl ;
      constrs: (Path.t * constructor_or_exception) Ident.tbl;
      labels: label_description Ident.tbl;
      types: (Path.t * type_declaration) Ident.tbl ;
      levels: (Path.t * level_declaration) Ident.tbl;
      modules: (Path.t * module_type) Ident.tbl;
      modtypes: (Path.t * modtype_declaration) Ident.tbl;
      components: (Path.t * module_components) Ident.tbl
    } 

and module_components = 
    Structure_comps of structure_components
  | Functor_comps of functor_components

and structure_components = {
    mutable comp_values:
      (string, (value_description option * int)) Tbl.t;
    mutable comp_constrs:
      (string, (constructor_or_exception * int)) Tbl.t;
    mutable comp_labels:
      (string, (label_description * int)) Tbl.t;
    mutable comp_types:
      (string, (type_declaration * int)) Tbl.t;
    mutable comp_levels:
      (string, (level_declaration * int)) Tbl.t;
    mutable comp_modules:
      (string, (module_type * int)) Tbl.t;
    mutable comp_modtypes: 
      (string, (modtype_declaration * int)) Tbl.t;
    mutable comp_components:
      (string, (module_components * int)) Tbl.t
}

and functor_components = {
    fcomp_param: Ident.t;                                  (* Formal parameter *)
    fcomp_arg: module_type;                              (* Argument signature *)
    fcomp_res: module_type;                                (* Result signature *)
    fcomp_env: t;     (* Environment in which the result signature makes sense *)
    fcomp_subst: Subst.t    (* Prefixing substitution for the result signature *)
}




let empty =
  { values = Ident.empty;
    constrs = Ident.empty;
    labels = Ident.empty;
    types = Ident.empty;
    levels = Ident.empty;
    modules = Ident.empty;
    modtypes = Ident.empty;
    components = Ident.empty
  } 


let comp_empty () =
  { comp_values = Tbl.empty; 
    comp_constrs = Tbl.empty;
    comp_labels = Tbl.empty; 
    comp_types = Tbl.empty;
    comp_levels = Tbl.empty;
    comp_modules = Tbl.empty; 
    comp_modtypes = Tbl.empty;
    comp_components = Tbl.empty; 
  }



(***************************************************************************)
(** {2 Persistent structures} *)

let components_of_module' =
  ref ((fun env sub path mty -> assert false) :
          t -> Subst.t -> Path.t -> module_type -> module_components)
let components_of_functor_appl =
  ref ((fun f p1 p2 -> assert false) :
          functor_components -> Path.t -> Path.t -> module_components)
let check_modtype_inclusion =
  (* to be filled with Includemod.check_modtype_inclusion *)
  ref ((fun env mty1 mty2 -> assert false) :
          t -> module_type -> module_type -> unit)

type pers_struct =
  { ps_name: string;
    ps_intf: interface;
    ps_comps: module_components;
    ps_crcs: (string * Digest.t) list;
    ps_filename: string }

let persistent_structures =
  (Hashtbl.create 17 : (string, pers_struct) Hashtbl.t)

(* Return the list of imported interfaces with their CRCs *)

let imported_units() =
  let imported_units =
    ref ([] : (string * Digest.t) list) in
  let units_xref =
    (Hashtbl.create 13 : (string, Digest.t * string) Hashtbl.t) in
  let add_unit source (name, crc) =
    try
      let (oldcrc, oldsource) = Hashtbl.find units_xref name in
      if oldcrc <> crc then
        raise(Error(Inconsistent_import(name, oldsource, source)))
    with Not_found ->
      Hashtbl.add units_xref name (crc, source);
      imported_units := (name, crc) :: !imported_units in
  Hashtbl.iter
    (fun name ps -> List.iter (add_unit ps.ps_filename) ps.ps_crcs)
    persistent_structures;
  !imported_units

let save_interface int modname filename =
  let comps =
    !components_of_module' empty Subst.identity
      (Pident(Ident.create_persistent modname))
      (Tmty_signature int.int_signature) 
  in
  let oc = open_out_bin filename in
  try
    output_string oc Config.magic_int;
    output_value oc (modname, int);
    flush oc;
    let crc = Digest.file filename in
    let crcs = (modname, crc) :: imported_units() in
    output_value oc crcs;
    (* Enter signature in persistent table so that imported_unit()
       will also return its crc *)
    let ps =
      { ps_name = modname;
        ps_intf = int;
        ps_comps = comps;
        ps_crcs = crcs;
        ps_filename = filename } in
    Hashtbl.add persistent_structures modname ps;
    close_out oc
  with exn ->
    close_out oc;
    Misc.remove_file filename;
    raise exn

let read_pers_struct modname filename =
  let ic = open_in_bin filename in
  try
    let buffer = String.create (String.length Config.magic_int) in
    really_input ic buffer 0 (String.length Config.magic_int);
    if buffer <> Config.magic_int then begin
      close_in ic;
      raise(Error(Not_an_interface filename))
    end;
    let (name, int) = input_value ic in
    let crcs = input_value ic in
    close_in ic;
    let comps =
      !components_of_module' empty Subst.identity
                             (Pident(Ident.create_persistent name))
                             (Tmty_signature int.int_signature) in
    let ps = { ps_name = name;
	       ps_intf = int;
               ps_comps = comps;
               ps_crcs = crcs;
               ps_filename = filename } in
    if ps.ps_name <> modname then
      raise(Error(Illegal_renaming(ps.ps_name, filename)));
    Hashtbl.add persistent_structures modname ps;
    ps
  with End_of_file | Failure _ ->
    close_in ic;
    raise(Error(Corrupted_interface(filename)))

let find_pers_struct name =
  try
    Hashtbl.find persistent_structures name
  with Not_found ->
    read_pers_struct name
      (Misc.find_in_path !Config.load_path (String.uncapitalize name ^ 
					    Config.ext_int))




let reset_cache() =
  Hashtbl.clear persistent_structures



(***************************************************************************)
(** {2 Lookup by identifier [find]} *)

let rec find_module_descr path env =
  match path with
    Pident id ->
      begin try
        let (p, desc) = Ident.find_same id env.components
        in desc
      with Not_found ->
        if Ident.persistent id
        then (find_pers_struct (Ident.name id)).ps_comps
        else raise Not_found
      end
  | Pdot(p, s, _) ->
      begin match find_module_descr p env with
        Structure_comps c ->
          let (descr, _) = Tbl.find s c.comp_components in
          descr
      | Functor_comps f ->
         raise Not_found
      end
  | Papply(p1, p2) ->
      begin match find_module_descr p1 env with
        Functor_comps f ->
          !components_of_functor_appl f p1 p2
      | Structure_comps c ->
          raise Not_found
      end



let find proj_env proj_comp path env =
  match path with
    Pident id ->
      let (_, data) = Ident.find_same id (proj_env env) in
      data
  | Pdot (p, s, _) ->
      begin match find_module_descr p env with
	Structure_comps c ->
	  let (data, _) = Tbl.find s (proj_comp c) in 
	  data
      |	Functor_comps _ ->
	  raise Not_found 
      end
  | Papply(p1, p2) ->
      raise Not_found



let find_value =
  find (fun env -> env.values) (fun sc -> sc.comp_values)
and find_type =
  find (fun env -> env.types) (fun sc -> sc.comp_types)
and find_level =
  find (fun env -> env.levels) (fun sc -> sc.comp_levels)
and find_exception path env =
  match
    find (fun env -> env.constrs) (fun sc -> sc.comp_constrs) path env
  with
    Constructor _ -> raise Not_found
  | Exception x -> x
and find_modtype =
  find (fun env -> env.modtypes) (fun sc -> sc.comp_modtypes)



(***************************************************************************)
(** {2 Finding by name [lookup]} *)

let rec lookup_module_descr loc lid env =
  match lid with
    Lident s ->
      begin try
        Ident.find_name s env.components
      with Not_found ->
        let ps = find_pers_struct s in
        (Pident(Ident.create_persistent s), ps.ps_comps)
      end
  | Ldot(l, s) ->
      let (p, descr) = lookup_module_descr loc l env in
      begin match descr with
        Structure_comps c ->
          let (data, pos) = Tbl.find s c.comp_components in
          (Pdot(p, s, pos), data)
      | Functor_comps f ->
         raise Not_found
      end
  | Lapply(l1, l2) ->
      let (p1, desc1) = lookup_module_descr loc l1 env in
      let (p2, mty2) = lookup_module loc l2 env in
      begin match desc1 with
        Functor_comps f ->
          !check_modtype_inclusion env mty2 f.fcomp_arg;
          (Papply(p1, p2), !components_of_functor_appl f p1 p2)
      | Structure_comps c ->
          raise Not_found
      end


and lookup_module loc lid env =
  try
    match lid with
      Lident s ->
	begin try
	  Ident.find_name s env.modules
	with Not_found ->
          let ps = find_pers_struct s in
          (Pident(Ident.create_persistent s), 
	   Tmty_signature ps.ps_intf.int_signature)
	end
    | Ldot(l, s) ->
	let (p, descr) = lookup_module_descr loc l env in
	begin match descr with
          Structure_comps c ->
            let (data, pos) = Tbl.find s c.comp_modules in
            (Pdot(p, s, pos), data)
	| Functor_comps f ->
            raise Not_found
	end
  | Lapply(l1, l2) ->
      let (p1, desc1) = lookup_module_descr loc l1 env in
      let (p2, mty2) = lookup_module loc l2 env in
      let p = Papply(p1, p2) in
      begin match desc1 with
        Functor_comps f ->
          !check_modtype_inclusion env mty2 f.fcomp_arg;
          (p, Subst.modtype (Subst.add_module f.fcomp_param p2 f.fcomp_subst)
                            f.fcomp_res)
      | Structure_comps c ->
          raise Not_found
      end
  with
    Not_found -> raise (LookupError (loc, Unbound_module lid))



let lookup loc err proj1 proj2 lid env =
  try
    match lid with
      Lident s ->
	Ident.find_name s (proj1 env)
    | Ldot(l, s) ->
	let (p, desc) = lookup_module_descr loc l env in
	begin match desc with
          Structure_comps c ->
            let (data, pos) = Tbl.find s (proj2 c) in
            (Pdot(p, s, pos), data)
	| Functor_comps f ->
            raise Not_found
	end
  | Lapply(l1, l2) ->
      raise Not_found
  with
    Not_found -> raise (LookupError (loc, err))



let lookup_simple loc err proj1 proj2 lid env =
  try
    match lid with
      Lident s ->
	Ident.find_name s (proj1 env)
    | Ldot(l, s) ->
	let (p, desc) = lookup_module_descr loc l env in
	begin match desc with
          Structure_comps c ->
            let (data, pos) = Tbl.find s (proj2 c) in
            data
	| Functor_comps f ->
            raise Not_found
	end
  | Lapply(l1, l2) ->
      raise Not_found
  with
    Not_found -> raise (LookupError (loc, err))



let lookup_value loc lid env =
  lookup loc (Unbound_value lid)
    (fun env -> env.values) (fun sc -> sc.comp_values) lid env
and lookup_constructor loc lid env =
  match
    lookup loc (Unbound_constr lid)
      (fun env -> env.constrs) (fun sc -> sc.comp_constrs) lid env
  with
    _, Constructor x -> x
  | _, Exception _ -> raise (LookupError (loc, Unbound_constr lid))

and lookup_label loc lid env =
  lookup_simple loc (Unbound_label lid)
    (fun env -> env.labels) (fun sc -> sc.comp_labels) lid env
and lookup_type loc lid env =
  lookup loc (Unbound_type lid)
    (fun env -> env.types) (fun sc -> sc.comp_types) lid env
and lookup_level loc lid env =
  lookup loc (Unbound_level lid)
    (fun env -> env.levels) (fun sc -> sc.comp_levels) lid env
and lookup_exception loc lid env =
  match
    lookup loc (Unbound_exception lid)
      (fun env -> env.constrs) (fun sc -> sc.comp_constrs) lid env
  with
    _, Constructor _ -> raise (LookupError (loc, Unbound_exception lid))
  | path, Exception x -> path, x

and lookup_modtype loc lid env =
  lookup loc (Unbound_modtype lid)
    (fun env -> env.modtypes) (fun sc -> sc.comp_modtypes) lid env



(***************************************************************************)
(** {2 Insertion of bindings by identifiers + path} *)

let find_modtype_expansion path env =
  match find_modtype path env with
    Tmodtype_abstract     -> raise Not_found
  | Tmodtype_manifest mty -> mty

(* Expand manifest module type names at the top of the given module type *)

let rec scrape_modtype mty env =
  match mty with
    Tmty_ident path ->
      begin try
        scrape_modtype (find_modtype_expansion path env) env
      with Not_found ->
        mty
      end
  | _ -> mty

let constructors_of_type ty_path decl =
  match decl.type_repr with
    Type_variant cstrs -> cstrs
  | _ -> []

let labels_of_type ty_path decl =
  match decl.type_repr with
    Type_record labels -> labels
  | _ -> []

(* Given a signature and a root path, prefix all idents in the signature
   by the root path and build the corresponding substitution. *)

let rec prefix_idents root pos sub = function
    [] -> ([], sub)
  | Tsig_value(id, decl) :: rem ->
      let p = Pdot(root, Ident.name id, pos) in
      let nextpos = pos + 1 in
      let (pl, final_sub) = prefix_idents root nextpos sub rem in
      (p::pl, final_sub)
  | Tsig_type(id, decl) :: rem ->
      let p = Pdot(root, Ident.name id, nopos) in
      let (pl, final_sub) =
        prefix_idents root pos (Subst.add_type id p sub) rem in
      (p::pl, final_sub)
  | Tsig_level(id, decl) :: rem ->
      let p = Pdot(root, Ident.name id, nopos) in
      let (pl, final_sub) =
	prefix_idents root pos (Subst.add_level id p sub) rem in
      (p::pl, final_sub)
  | Tsig_exception(id, decl) :: rem ->
      let p = Pdot(root, Ident.name id, pos) in
      let (pl, final_sub) = 
	prefix_idents root (pos+1) (Subst.add_exception id p sub) rem in
      (p::pl, final_sub)
  | Tsig_module(id, mty) :: rem ->
      let p = Pdot(root, Ident.name id, pos) in
      let (pl, final_sub) =
        prefix_idents root (pos+1) (Subst.add_module id p sub) rem in
      (p::pl, final_sub)
  | Tsig_modtype(id, decl) :: rem ->
      let p = Pdot(root, Ident.name id, nopos) in
      let (pl, final_sub) =
        prefix_idents root pos
                      (Subst.add_modtype id (Tmty_ident p) sub) rem in
      (p::pl, final_sub)




let rec components_of_module env sub path mty =

  match scrape_modtype mty env with

    Tmty_signature sg ->

      let c = comp_empty () in

      let (pl, sub) = prefix_idents path 0 sub sg in

      let env = ref env in
      let pos = ref 0 in

      List.iter2 (fun item path ->

        match item with

          Tsig_value(id, decl) ->

            let decl' = Subst.value_description sub decl in
            c.comp_values <-
              Tbl.add (Ident.name id) (Some decl', !pos) c.comp_values;
	    incr pos

        | Tsig_type(id, decl) ->
            let decl' = Subst.type_declaration sub decl in
            c.comp_types <-
              Tbl.add (Ident.name id) (decl', nopos) c.comp_types;
            List.iter
              (fun (name, descr) ->
                c.comp_constrs <- Tbl.add name (Constructor descr, nopos)
		    c.comp_constrs)
              (constructors_of_type path decl');
            List.iter
              (fun (name, descr) ->
                c.comp_labels <- Tbl.add name (descr, nopos) c.comp_labels)
              (labels_of_type path decl'); 
            env := store_type_infos id path decl !env

        | Tsig_level(id, decl) ->
            let decl' = Subst.level_declaration sub decl in
            c.comp_levels <-
              Tbl.add (Ident.name id) (decl, !pos) c.comp_levels;
            incr pos

        | Tsig_exception(id, decl) ->
            let decl' = Subst.exception_declaration sub decl in
            c.comp_constrs <-
              Tbl.add (Ident.name id) (Exception decl, !pos) c.comp_constrs;
            incr pos

        | Tsig_module(id, mty) ->
            let mty' = Subst.modtype sub mty in
            c.comp_modules <-
              Tbl.add (Ident.name id) (mty', !pos) c.comp_modules;
            let comps = components_of_module !env sub path mty in
            c.comp_components <-
              Tbl.add (Ident.name id) (comps, !pos) c.comp_components;
            env := store_module id path mty !env;
            incr pos

        | Tsig_modtype(id, decl) ->
            let decl' = Subst.modtype_declaration sub decl in
            c.comp_modtypes <-
              Tbl.add (Ident.name id) (decl', nopos) c.comp_modtypes;
            env := store_modtype id path decl !env

      ) sg pl;

      Structure_comps c



  | Tmty_functor(param, ty_arg, pci, pcf, ty_res) ->
        Functor_comps {
          fcomp_param = param;
          (* fcomp_arg must be prefixed eagerly, because it is interpreted
             in the outer environment, not in env *)
          fcomp_arg = Subst.modtype sub ty_arg;
          (* fcomp_res is prefixed lazily, because it is interpreted in env *)
          fcomp_res = ty_res;
          fcomp_env = env;
          fcomp_subst = sub }



  | Tmty_ident p ->
      Structure_comps (comp_empty ())




and store_value id path decl env =
  { env with
    values = Ident.add id (path, decl) env.values
  } 

and store_type_infos id path info env =
  (* Simplified version of store_type that doesn't compute and store
     constructor and label infos, but simply record the arity and
     manifest-ness of the type.  Used in components_of_module to
     keep track of type abbreviations (e.g. type t = float) in the
     computation of label representations. *)
  { env with
    types = Ident.add id (path, info) env.types
  } 

and store_type id path info env =
  { env with
    constrs =
      List.fold_right
        (fun (name, descr) constrs ->
	  let id = Ident.create name in
          Ident.add id (Pident id, Constructor descr) constrs)
        (constructors_of_type path info)
        env.constrs;
    labels =
      List.fold_right
        (fun (name, descr) labels ->
          Ident.add (Ident.create name) descr labels)
        (labels_of_type path info)
        env.labels;
    types = Ident.add id (path, info) env.types
  }

and store_level id path decl env =
  { env with
    levels = Ident.add id (path, decl) env.levels
  } 

and store_exception id path decl env =
  { env with
    constrs = Ident.add id (path, Exception decl) env.constrs
  } 

and store_module id path mty env =
  { env with
    modules = Ident.add id (path, mty) env.modules;
    components =
      Ident.add id (path, components_of_module env Subst.identity path mty)
      env.components
  } 


and store_modtype id path info env =
  { env with
    modtypes = Ident.add id (path, info) env.modtypes
  } 

let _ = components_of_module' := components_of_module

(* Memoized function to compute the components of a functor application
   in a path. *)

let funappl_memo =
  (Hashtbl.create 17 : (Path.t, module_components) Hashtbl.t)

let _ =
  components_of_functor_appl :=
    (fun f p1 p2 ->
      let p = Papply(p1, p2) in
      try
        Hashtbl.find funappl_memo p
      with Not_found ->
        let mty = 
          Subst.modtype (Subst.add_module f.fcomp_param p2 Subst.identity)
                        f.fcomp_res in
        let comps = components_of_module f.fcomp_env f.fcomp_subst p mty in
        Hashtbl.add funappl_memo p comps;
        comps)



(***************************************************************************)
(** {2 Insertion of bindings by identifiers} *)

let add_value id desc env =
  store_value id (Pident id) desc env

let add_type id desc env =
  store_type id (Pident id) desc env

let add_level id desc env =
  store_level id (Pident id) desc env

let add_exception id desc env =
  store_exception id (Pident id) desc env

let add_module id mty env =
  store_module id (Pident id) mty env

let add_modtype id mty env =
  store_modtype id (Pident id) mty env



(***************************************************************************)
(** {2 Insertion of bindings by names} *)

let enter store_fun name data env =
  let id = Ident.create name in (id, store_fun id (Pident id) data env)

let enter_value = enter store_value
let enter_type = enter store_type
let enter_level = enter store_level
let enter_exception = enter store_exception
let enter_module = enter store_module
let enter_modtype = enter store_modtype



(* TEMPORARY Hack ! *)

let add_patenv patenv env =

  let values = 
    Pat_context.fold (fun _ (id, _) values ->
      Ident.add id (Pident id, None) values
    ) patenv env.values
  in

  { env with values = values } 

let add_local_binding id env =
  { env with values = Ident.add id (Pident id, None) env.values }




(* Insertion of all components of a signature *)

let add_item comp env =
  match comp with
    Tsig_value(id, decl)     -> add_value id (Some decl) env
  | Tsig_type(id, decl)      -> add_type id decl env
  | Tsig_level(id, decl)     -> add_level id decl env
  | Tsig_exception(id, decl) -> add_exception id decl env
  | Tsig_module(id, mty)     -> add_module id mty env
  | Tsig_modtype(id, decl)   -> add_modtype id decl env

let rec add_signature sg env =
  match sg with
    [] -> env
  | comp :: rem -> add_signature rem (add_item comp env)




(***************************************************************************)
(** {2 Open a signature path} *)

let open_signature root sg env =
  (* First build the paths and substitution *)
  let (pl, sub) = prefix_idents root 0 Subst.identity sg in
  (* Then enter the components in the environment after substitution *)
  let newenv =
    List.fold_left2
      (fun env item p ->
        match item with
          Tsig_value(id, decl) ->
            store_value (Ident.hide id) p
                        (Some (Subst.value_description sub decl)) env
        | Tsig_type(id, decl) ->
            store_type (Ident.hide id) p
                       (Subst.type_declaration sub decl) env
	| Tsig_level(id, decl) ->
	    store_level (Ident.hide id) p
	                (Subst.level_declaration sub decl) env
        | Tsig_exception(id, decl) ->
            store_exception (Ident.hide id) p
                            (Subst.exception_declaration sub decl) env
        | Tsig_module(id, mty) ->
            store_module (Ident.hide id) p (Subst.modtype sub mty) env
        | Tsig_modtype(id, decl) ->
            store_modtype (Ident.hide id) p
                          (Subst.modtype_declaration sub decl) env
    ) env sg pl
  in
  newenv
  

 
(* Open a signature from a file *)

let open_pers_signature name env =
  let ps = find_pers_struct name in
  open_signature (Pident(Ident.create_persistent name)) 
    ps.ps_intf.int_signature env

(* Read a signature from a file *)

let read_interface modname filename =
  let ps = read_pers_struct modname filename in
  ps.ps_intf

(* Return the CRC of the interface of the given compilation unit *)

let crc_of_unit name =
  let ps = find_pers_struct name in
  try
    List.assoc name ps.ps_crcs
  with Not_found ->
    assert false



(***************************************************************************)
(** {2 Current environment} *)

let current_env = 
  ref empty

let set_current_env env =
  current_env := env

let current_lat =
  ref (Principal.create ())

let set_current_lat lat =
  current_lat := lat


(***************************************************************************)
(** {2 Comparing levels} *)

(** [level_leq level1 level2] returns a boolean indicating wether
    the level [level1] is less than or equal to [level2] in the current
    environment and lattice.
 *)
let level_leq level1 level2 =

  match level1, level2 with
    Tlvl_principal name1, Tlvl_principal name2 -> 
	Principal.leq !current_lat name1 name2
  | Tlvl_path path1, Tlvl_principal name2 ->
      let lvd1 = find_level path1 !current_env in
      Level.Set.exists (function 
	  Tlvl_principal name1 -> Principal.leq !current_lat name1 name2
	| Tlvl_path _ -> false
      ) lvd1.lvd_ub_closed
  | Tlvl_principal name1, Tlvl_path path2 ->
      let lvd2 = find_level path2 !current_env in
      Level.Set.exists (function
	  Tlvl_principal name2 -> Principal.leq !current_lat name1 name2
	| Tlvl_path _ -> false
      ) lvd2.lvd_lb_closed
  | Tlvl_path path1, Tlvl_path path2 ->
      let lvd1 = find_level path1 !current_env
      and lvd2 = find_level path2 !current_env in
      Level.Set.exists (function
	  Tlvl_principal name1 ->
	    Level.Set.exists (function
		Tlvl_principal name2 -> Principal.leq !current_lat name1 name2
	      |	Tlvl_path _ -> false
            ) lvd2.lvd_lb_closed
	| Tlvl_path path1' -> 
	    Level.Set.exists (function
		Tlvl_principal _ -> false
	      |	Tlvl_path path2' -> Path.same path1' path2'
	    ) lvd2.lvd_lb_closed
      ) lvd1.lvd_ub_closed
