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

(* $Id: type_core.ml,v 1.15 2005/05/16 15:43:24 simonet Exp $ *)

(* Type inference for the core language *)

open Format
open Dalton_aux
open Asttypes
open Parsetree
open Path
open Bound
open Types
open Typedtree
open Longident
module Report = Solver.Report




(***************************************************************************)
(** {2 Solver for let-definitions} *)

type let_definition = 
  { let_desc: (pattern * expression) list;
    let_cset: Solver.cset;
    let_gen: (Ident.t * value_description) list;
    let_ng: Solver.node Pat_context.t;
    let_context: Solver.node Expr_context.t;
    let_pc: Solver.node;
    let_row: Solver.node;
    let_env: Env.t
  }

module LetDefinition = Solver.Scheme (struct

  open Dalton_aux

  type t = let_definition

  let cset root =
    root.let_cset

  let copy cset' f root =
    { root with
      let_cset = cset';
      let_ng = Pat_context.map f root.let_ng;
      let_context = Expr_context.map f root.let_context;
      let_pc = f root.let_pc;
      let_row = f root.let_row
    } 

  let iter f root =
    f Contravariant root.let_pc;
    f Covariant root.let_row;
    Pat_context.iter (f Covariant) root.let_ng;
    Expr_context.iter (f Contravariant) root.let_context

  let iter2 f root1 root2 =
    assert false

  let fprint ppf f_cset f_node root =
    fprintf ppf "@[<v>pc: %a@ row: %a@ %a@]"
      (f_node Contravariant) root.let_pc
      (f_node Covariant) root.let_row
      f_cset root.let_cset

end)



type eval_phrase = 
  { eval_cset: Solver.cset;
    eval_typ: Solver.cset;
    eval_pc: Solver.node;
    eval_row: Solver.node;
  }

module Expression = Solver.Scheme (struct

  open Dalton_aux

  type t = expression

  let cset root =
    root.exp_cset

  let copy _ _ _ = assert false

  let iter f root =
    Expr_context.iter (f Contravariant) root.exp_context;
    f Contravariant root.exp_pc;
    f Covariant root.exp_typ;
    f Covariant root.exp_row

  let iter2 f root1 root2 =
    assert false

  let fprint ppf f_cset f_node root =
    assert false

end)



(***************************************************************************)
(** {2 Error repors} *)

type error =
    Multiply_bound_variable of string
  | Orpat_vars of string
  | Constructor_arity_mismatch of Longident.t * int * int
  | Label_mismatch of  Longident.t * Path.t * Path.t
  | Label_multiply_defined of string
  | Label_missing of string list
  | Label_not_mutable of Longident.t
  | Illegal_letrec_pat
  | Illegal_letrec_expr
  | Illegal_try_pattern
  | UnificationError
    of Solver.unification_report * Solver.Report.unification_message
  | SolveValue of Value_description.solve_report
  | SolveLet of LetDefinition.solve_report
  | SolveExpr of Expression.solve_report
  | Non_generalizable of Ident.t * Value_description.minimal_report
  | Initialize of Level.Set.t * Level.Set.t



exception Error of Location.t list * error

let error (loc, err) = raise (Error ([loc], err))
let error_list (loc_list, err) = raise (Error (loc_list, err))

let report_error ppf = function

  | Orpat_vars name ->
      fprintf ppf "Variable %s must occur on both sides of this | pattern"
        name

  | Multiply_bound_variable name ->
      fprintf ppf "The variable %s is bound several times in this matching"
        name

  | Constructor_arity_mismatch (lid, expected, provided) ->
      fprintf ppf
       "@[The constructor %a@ expects %i argument(s),@ \
        but is here applied to %i argument(s)@]"
       Longident.fprint lid expected provided

  | Label_mismatch(lid, p1, p2) ->
      fprintf ppf
        "@[The record field label %a@ belongs to the type %a@ \
        but is here mixed with labels of type %a@]"
        Longident.fprint lid
        Path.fprint p1
        Path.fprint p2

  | Label_multiply_defined name ->
      fprintf ppf "The record field label %s is defined several times"
        name

  | Label_missing labels ->
      let print_labels ppf = List.iter (fun lbl -> fprintf ppf "@ %s" lbl) in
      fprintf ppf "@[<hov>Some record field labels are undefined:%a@]"
        print_labels labels

  | Label_not_mutable lid ->
      fprintf ppf "The record field label %a is not mutable" 
        Longident.fprint lid

  | Illegal_letrec_pat ->
      fprintf ppf
        "Only variables are allowed as left-hand side of `let rec'"

  | Illegal_letrec_expr ->
      fprintf ppf
        "This kind of expression is not allowed as right-hand side of `let rec'"

  | Illegal_try_pattern ->
      fprintf ppf 
        "This kind of pattern is not allowed in a \"try ... with\" expression."

  | UnificationError (report, msg) ->
      Solver.Report.unification_message := msg;
      Solver.report_unification ppf report

  | SolveValue report ->
      Value_description.report_solve ppf report

  | SolveLet report ->
      LetDefinition.report_solve ppf report

  | SolveExpr report ->
      Expression.report_solve ppf report

  | Non_generalizable (id, report) ->
      fprintf ppf
      "@[<v>@[The type scheme of %a:%a@ \
	You should assign them by some type constraint.@]@]"
	Ident.fprint id
	Value_description.report_minimal report

  | Initialize (lb_set, ub_set) ->
      fprintf ppf 
	"@[<v>This expression is executed in a context of level %a@ \
	but has an effect at level %a.@ This yields the following information \
	flow:@ @[<v 2>  %t@]@ which is not legal.@]"
        Level.Set.fprint lb_set 
        Level.Set.fprint ub_set
        (fun ppf -> Solver.Lub.fprint_notleq ppf lb_set ub_set)
	


(***************************************************************************)
(** {2 Interface with the solver} *)

(*-------------------------------------------------------------------------*)
(** {3 Generation of nodes} *)

let merge cset1 cset2 =
  Solver.merge_cset cset1 cset2

let mkcset () =
  Solver.cset ()

let level_variable cset = 
  Solver.variable cset Katom

let type_variable cset = 
  Solver.variable cset Ktype

let row_variable cset = 
  Solver.variable cset (Krow Katom)

let mktyp cset t =
  Solver.typ cset t

let mkrow cset (exn, lvl, row') = 
  Solver.row cset (exn, lvl, row')



(*-------------------------------------------------------------------------*)
(** {3 Generation of constraints} *)

let ( < ) nd1 nd2 =
  Solver.strong_leq nd1 nd2

let ( <& ) nd1 nd2 =
  Solver.weak_leq (Solver.Nd nd1) (Solver.Nd nd2)

let ( <&. ) nd1 ub =
  Solver.upper_bound (Solver.Nd nd1) ub

let (<|) nd1 nd2 =
  Solver.weak_leq (Solver.Nd nd1) (Solver.Nd nd2)



let ( << ) nd1 nd2 loc message =
  try
    nd1 < nd2
  with
    Solver.UnificationError report -> 
      error (loc, UnificationError (report, message))



(*-------------------------------------------------------------------------*)
(** {3 Unions and intersections} *)

let ( + ) nd1 nd2 =
  let nd = Solver.variable_in_sk nd1 in
  nd1 < nd;
  nd2 < nd;
  nd

let ( ++ ) nd1 nd2 loc message =
  try
    nd1 + nd2
  with
    Solver.UnificationError report -> 
      error (loc, UnificationError (report, message))

let ( * ) nd1 nd2 =
  let nd = Solver.variable_in_sk nd1 in
  nd < nd1;
  nd < nd2;
  nd

let ( ** ) nd1 nd2 loc message =
  try
    nd1 * nd2
  with
    Solver.UnificationError report -> 
      error (loc, UnificationError (report, message))



(***************************************************************************)
(** {2 Auxilliary checks} *)

(*-------------------------------------------------------------------------*)
(** {3 Constructors} *)

let rec check_pattern_arity i ppat =
  match ppat.ppat_desc with
    Ppat_any -> None
  | Ppat_tuple list -> 
      let len = List.length list in
      if i = 1 || i = len then None else Some len
  | Ppat_paren ppat' -> check_pattern_arity i ppat'
  | _ -> if i = 1 then None else Some 1

let rec check_expr_arity i pexp =
  match pexp.pexp_desc with
    Pexp_tuple list -> 
      let len = List.length list in
      if i = 1 || i = len then None else Some len
  | Pexp_paren pexp' -> check_expr_arity i pexp'
  | _ -> if i = 1 then None else Some 1

let check_arity_option i test_arity opt =
  match opt with
    None -> if i = 0 then None else Some 0
  | Some x -> test_arity i x



let check_cstr_pattern loc lid cstr_decl opt_ppat =
  match check_arity_option cstr_decl.cstr_arity check_pattern_arity  opt_ppat
  with
    None -> ()
  | Some provided ->
      error (loc, 
	     Constructor_arity_mismatch (lid, cstr_decl.cstr_arity, provided))

let check_exn_pattern loc lid exn_decl opt_ppat =
  match check_arity_option exn_decl.exn_arity check_pattern_arity  opt_ppat
  with
    None -> ()
  | Some provided ->
      error (loc, 
	     Constructor_arity_mismatch (lid, exn_decl.exn_arity, provided))

let check_cstr_expr loc lid cstr_decl opt_pexp =
  match check_arity_option cstr_decl.cstr_arity check_expr_arity  opt_pexp
  with
    None -> ()
  | Some provided ->
      error (loc, 
	     Constructor_arity_mismatch (lid, cstr_decl.cstr_arity, provided))

let check_exn_expr loc lid exn_decl opt_pexp =
  match check_arity_option exn_decl.exn_arity check_expr_arity  opt_pexp
  with
    None -> ()
  | Some provided ->
      error (loc, 
	     Constructor_arity_mismatch (lid, exn_decl.exn_arity, provided))



let tuplify_argument cset list =
  match list with
    [] -> assert false
  | [typ] -> typ
  | list -> mktyp cset (Predef.tuple list)



(*-------------------------------------------------------------------------*)
(** {2 Records} *)

let check_record strict loc lbl_list =

  let names =
    match lbl_list with
      [] -> assert false
    | (lbl, _) :: _ -> lbl.lbl_all
  in

  (* Checking the absence of multiples definitions. *)

  let defined = Array.make (Array.length names) false in

  List.iter (function lbl, _ ->
    if defined.(lbl.lbl_pos) 
    then error (loc, Label_multiply_defined names.(lbl.lbl_pos))
    else defined.(lbl.lbl_pos) <- true
  ) lbl_list;

  (* Checking the presence of all labels *)

  if strict then begin
    let miss = ref [] in
    for i = Array.length defined - 1 downto 0 do
      if not defined.(i) then miss := names.(i) :: !miss
    done;
    if !miss <> [] then error (loc, Label_missing !miss)
  end



(*-------------------------------------------------------------------------*)
(** {3 Let-definitions} *)

let check_letrec ppat_pexp_list =

  List.iter (function ppat, pexp ->

    begin match pexp.pexp_desc with
      Pexp_function _ -> ()
    | _ -> error (pexp.pexp_loc, Illegal_letrec_expr)
    end;

    begin match ppat.ppat_desc with
      Ppat_var _ -> ()
    | _ -> error (pexp.pexp_loc, Illegal_letrec_pat)
    end

  ) ppat_pexp_list



let rec generalizable exp =

  match exp.exp_desc with

    Texp_ident _
  | Texp_constant _
  | Texp_function _
  | Texp_construct (_, None) -> true

  | Texp_tuple exp_list -> List.for_all generalizable exp_list

  | Texp_record (lbl_exp_list, opt_exp) ->
      List.for_all
	(function lbl, exp -> lbl.lbl_mut = Immutable && generalizable exp)
	lbl_exp_list
	&&
      (match opt_exp with None -> true | Some exp' -> generalizable exp')

  | Texp_field (exp', _)
  | Texp_construct (_, Some exp') -> generalizable exp'

  | Texp_let _
  | Texp_apply _
  | Texp_match _
  | Texp_raise _
  | Texp_try _
  | Texp_finally _
  | Texp_setfield _
  | Texp_array _
  | Texp_ifthenelse _
  | Texp_sequence _
  | Texp_while _
  | Texp_for _
  | Texp_when _
  | Texp_assert _
  | Texp_assertfalse -> false



(***************************************************************************)
(** {2 Typing patterns} *)

let pattern_context_sum loc patenv1 patenv2 =

  let patenv2, patenv =

    Pat_context.fold (fun name (id, typ1) (patenv2, patenv) ->

      try
        let _, typ2 = Pat_context.find name patenv2 in
        (Pat_context.remove name patenv2,
         Pat_context.add name
           (id, (typ1 ++ typ2) loc (Report.Or_pattern_var name)) patenv)
      with
        Not_found -> error (loc, Orpat_vars (Ident.name id))

    ) patenv1 (patenv2, Pat_context.empty)

  in

  Pat_context.iter_id (function id ->
    error (loc, Orpat_vars (Ident.name id))
  ) patenv2;

  patenv



let pattern_sum loc pat1 pat2 =

  merge pat1.pat_cset pat2.pat_cset;

  let patenv = pattern_context_sum loc pat1.pat_context pat2.pat_context in

  pat1.pat_cset, patenv, (pat1.pat_level + pat2.pat_level)



let pattern_product loc tpat_list =

  let cset = mkcset () in
  let lvl = level_variable cset in

  let patenv =

    List.fold_left (fun patenv tpat ->

      merge cset tpat.pat_cset;
      tpat.pat_level < lvl;

      Pat_context.fold (fun name (id, typ) patenv ->
        if Pat_context.mem name patenv then
          error (loc, Multiply_bound_variable (Ident.name id));
        Pat_context.add name (id, typ) patenv
      ) tpat.pat_context patenv

    ) Pat_context.empty tpat_list

  in

  (cset, patenv, lvl)



let rec type_pattern trypat env ppat =

  match ppat.ppat_desc with

    Ppat_any ->
      let cset = mkcset () in
      { pat_desc = Tpat_any;
        pat_loc = ppat.ppat_loc;
        pat_cset = cset;
        pat_context = Pat_context.empty;
        pat_typ = type_variable cset;
        pat_level = level_variable cset;
        pat_env = env
      } 



  | Ppat_var name ->
      let cset = mkcset () in
      let typ = type_variable cset in
      let id = Ident.create name in
      { pat_desc = Tpat_var id;
        pat_loc = ppat.ppat_loc;
        pat_cset = cset;
        pat_context = Pat_context.singleton name (id, typ);
        pat_typ = typ;
        pat_level = level_variable cset;
        pat_env = env
      } 



  | Ppat_alias (ppat', name) ->
      let pat' = type_pattern trypat env ppat' in
      if Pat_context.mem name pat'.pat_context then
        error (ppat.ppat_loc, Multiply_bound_variable name);
      let id = Ident.create name in
      { pat' with
        pat_loc = ppat.ppat_loc;
        pat_context = Pat_context.add name (id, pat'.pat_typ) pat'.pat_context
      }



  | Ppat_constant c ->
      if trypat then error (ppat.ppat_loc, Illegal_try_pattern);
      let cset = mkcset () in
      let lvl = level_variable cset in
      let typ =
	match c with
	  Const_int _ -> 
	    mktyp cset (Predef.int lvl)
	| Const_char _ -> 
	    mktyp cset (Predef.char lvl)
	| Const_string _ ->
	    mktyp cset (Predef.string lvl)
	| Const_float _ -> 
	    mktyp cset (Predef.float lvl)
	| Const_charray _ ->
	    mktyp cset (Predef.charray lvl lvl)
      in
      { pat_desc = Tpat_constant c;
        pat_loc = ppat.ppat_loc;
        pat_cset = cset;
        pat_context = Pat_context.empty;
        pat_typ = typ;
        pat_level = lvl;
        pat_env = env
      }



  | Ppat_tuple list ->
      let pat_list = List.map (type_pattern trypat env) list in
      let cset, patenv, lvl = pattern_product ppat.ppat_loc pat_list in
      let typ_res =
        mktyp cset (Predef.tuple (List.map (fun pat' -> pat'.pat_typ) pat_list))
      in
      { pat_desc = Tpat_tuple pat_list;
        pat_loc = ppat.ppat_loc;
        pat_cset = cset;
        pat_context = patenv;
        pat_typ = typ_res;
        pat_level = lvl;
        pat_env = env
      } 



  | Ppat_construct (lid, opt_ppat', b) ->
      if trypat then error (ppat.ppat_loc, Illegal_try_pattern);

      let cstr = 
        Constructor_description.copy 
          (Env.lookup_constructor ppat.ppat_loc lid env)
      in

      check_cstr_pattern ppat.ppat_loc lid cstr opt_ppat';

      let cset = cstr.cstr_cset in
      let lvl = level_variable cset in
      begin match cstr.cstr_level with
        None -> ()
      | Some lvl0 -> lvl0 < lvl
      end;

      let opt_pat', patenv =

        match opt_ppat' with

          None -> None, Pat_context.empty

        | Some ppat' ->
            let pat' = type_pattern trypat env ppat' in
            merge cset pat'.pat_cset;
            ((tuplify_argument cset cstr.cstr_args) << pat'.pat_typ)
              ppat'.ppat_loc Report.PatternR;
            pat'.pat_level < lvl;
            Some pat', pat'.pat_context

      in

      { pat_desc = Tpat_construct (cstr, opt_pat');
        pat_loc = ppat.ppat_loc;
        pat_cset = cset;
        pat_context = patenv;
        pat_typ = cstr.cstr_res;
        pat_level = lvl;
        pat_env = env
      } 



  | Ppat_record lid_ppat_list ->     

      let cset = mkcset () in
      let typ_res = type_variable cset in
      let lvl = level_variable cset in

      let lbl_pat_list =

	List.map (function lid, ppat ->

	  let lbl = 
	    Label_description.copy (Env.lookup_label ppat.ppat_loc lid env) 
	  in
	  let pat = type_pattern trypat env ppat in

	  merge lbl.lbl_cset cset;
	  merge pat.pat_cset cset;
          (lbl.lbl_arg << pat.pat_typ) pat.pat_loc Report.PatternR;
          begin match lbl.lbl_level with
            None -> ()
          | Some lvl0 -> lvl0 < lvl
          end;
          (typ_res << lbl.lbl_res) ppat.ppat_loc Report.PatternL;

	  lbl, pat

        ) lid_ppat_list

      in

      check_record false ppat.ppat_loc lbl_pat_list;

      let _, patenv, lvl' = 
        pattern_product ppat.ppat_loc (List.map snd lbl_pat_list)
      in

      lvl' < lvl;


      { pat_desc = Tpat_record lbl_pat_list;
        pat_loc = ppat.ppat_loc;
        pat_cset = cset;
        pat_context = patenv;
        pat_typ = typ_res;
        pat_level = lvl;
        pat_env = env
      } 



  | Ppat_array ppat_list ->
      if trypat then error (ppat.ppat_loc, Illegal_try_pattern);

      let pat_list = List.map (type_pattern trypat env) ppat_list in
      let cset, patenv, lvl = pattern_product ppat.ppat_loc pat_list in
      let lvl_array = level_variable cset
      and typ_array = type_variable cset
      in

      List.iter2 (fun pat ppat ->
	(typ_array << pat.pat_typ) ppat.ppat_loc Report.PatternR
      ) pat_list ppat_list;

      { pat_desc = Tpat_array pat_list;
        pat_loc = ppat.ppat_loc;
        pat_cset = cset;
        pat_context = patenv;
        pat_typ = mktyp cset (Predef.array typ_array lvl_array);
        pat_level = lvl + lvl_array;
        pat_env = env
      } 



  | Ppat_or (ppat1, ppat2) ->
      if trypat then error (ppat.ppat_loc, Illegal_try_pattern);

      let pat1 = type_pattern trypat env ppat1
      and pat2 = type_pattern trypat env ppat2 in

      let cset, patenv, lvl = pattern_sum ppat.ppat_loc pat1 pat2 in

      { pat_desc = Tpat_or (pat1, pat2, None);
        pat_loc = ppat.ppat_loc;
        pat_cset = cset;
        pat_context = patenv;
        pat_typ = (pat1.pat_typ ** pat2.pat_typ) ppat.ppat_loc Report.Or_pattern;
        (* Bug fix 2005-02-15 *)
        pat_level = lvl;
        pat_env = env
      } 



  | Ppat_constraint (ppat, ptys) -> 

      let pat = type_pattern trypat env ppat in
      let cset, typ = Transl_typeexpr.transl_scheme env ptys in
      merge pat.pat_cset cset;

      (typ << pat.pat_typ) pat.pat_loc Report.PatternR;
      { pat with pat_typ = typ }


  | Ppat_paren ppat' ->
      type_pattern trypat env ppat'



(***************************************************************************)
(** {2 Typing try-patterns} *)

let type_try_pattern env ptry = 

  match ptry.ptry_desc with

    Ptry_any ->
      let cset = mkcset () in
      let lvl = level_variable cset in
      let row = row_variable cset in
      row <& lvl;
      { try_desc = Ttry_any;
        try_loc = ptry.ptry_loc;
        try_cset = cset;
        try_context = Pat_context.empty;
        try_handled = row;
        try_reraise = row;
        try_remainder = row_variable cset;
        try_level = lvl;
        try_env = env
      } 


  | Ptry_list list ->

      let cset = mkcset () in
      let row_handled = row_variable cset in
      let lvl = level_variable cset in

      let patenv, row_reraise, row_remainder, list' =

        List.fold_left (fun (patenv, row_reraise, row_remainder, list') 
	    item ->

          let path, exn = 
            Env.lookup_exception item.ptryi_loc item.ptryi_exception env
          in

          let exn' = Exception_declaration.copy exn in
	  merge cset exn'.exn_cset;

          check_exn_pattern
	    item.ptryi_loc item.ptryi_exception exn' item.ptryi_arg;

          let patenv', opt_pat =
            match item.ptryi_arg with
              None -> Pat_context.empty, None
            | Some ppat ->
                let pat = type_pattern true env ppat in
                merge cset pat.pat_cset;
                (tuplify_argument cset exn'.exn_args << pat.pat_typ)
                  item.ptryi_loc Report.PatternR;
		(if list' = [] then pat.pat_context else
                pattern_context_sum item.ptryi_loc patenv pat.pat_context), 
		Some pat
          in

          let lvl0 = level_variable cset in
          row_handled < (mkrow cset (path, lvl0, row_variable cset));
          lvl0 < lvl;

          let row_remainder' =
            let row' = row_variable cset in
            row_remainder < mkrow cset (path, level_variable cset, row');
            mkrow cset (path, level_variable cset, row')

          and row_reraise' =
            row_reraise + (mkrow cset (path, lvl0, row_variable cset))
          in

          (patenv', row_reraise', row_remainder', (exn', opt_pat) :: list')

        ) (Pat_context.empty, row_variable cset, row_handled, []) list

      in

      { try_desc = Ttry_list (List.rev list');
        try_loc = ptry.ptry_loc;
        try_cset = cset;
        try_context = patenv;
        try_handled = row_handled;
        try_reraise = row_reraise;
        try_remainder = row_remainder;
        try_level = lvl;
        try_env = env
      } 




(***************************************************************************)
(** {2 Typing core expressions} *)

(*-------------------------------------------------------------------------*)
(** {3 Local context manipulation} *)

let ident_locations id exp =

  let locations = ref [] in

  let rec find exp =
    match exp.exp_desc with
      Texp_ident (Pident id', None) when Ident.same id id' -> 
	locations := exp.exp_loc :: !locations
    | Texp_ident _
    | Texp_constant _
    | Texp_assertfalse -> ()

    | Texp_let (_, pat_exp_list, exp) ->
	List.iter (function _, exp -> find exp) pat_exp_list
    | Texp_match (exp, pat_when_exp_list, _) ->
	List.iter (function _, opt_exp, exp -> 
	  Option.iter find opt_exp;
	  find exp
        ) pat_when_exp_list;
	find exp
    | Texp_function (pat_when_exp_list, _) ->
	List.iter (function _, opt_exp, exp -> 
	  Option.iter find opt_exp;
	  find exp
        ) pat_when_exp_list
    | Texp_apply (exp, exp_list) ->
	find exp;
	List.iter find exp_list
    | Texp_construct (_, opt_exp)
    | Texp_raise (_, opt_exp) ->
	(match opt_exp with None -> () | Some exp -> find exp)
    | Texp_try (exp, try_exp_list) ->
	find exp;
	List.iter (function _, exp, _ -> find exp) try_exp_list
    | Texp_finally (exp1, exp2)
    | Texp_setfield (exp1, _, exp2)
    | Texp_sequence (exp1, exp2)
    | Texp_while (exp1, exp2)
    | Texp_when (exp1, exp2) -> 
	find exp1;
	find exp2

    | Texp_record (lbl_exp_list, opt_exp) ->
	List.iter (function _, exp -> find exp) lbl_exp_list;
	(match opt_exp with None -> () | Some exp -> find exp)

    | Texp_tuple exp_list
    | Texp_array exp_list ->
	List.iter find exp_list

    | Texp_ifthenelse (exp1, exp2, opt_exp) ->
	find exp1; 
	find exp2;
	(match opt_exp with None -> () | Some exp -> find exp)
	  
    | Texp_for (_, exp1, exp2, _, exp3) ->
	find exp1;
	find exp2;
	find exp3

    | Texp_field (exp, _)
    | Texp_assert exp ->
	find exp

  in

  find exp;
  ! locations
      
      

let merge_context exp exp_expenv expenv =

  Expr_context.fold (fun id typ' expenv ->

    Expr_context.add id begin

      try
        (Expr_context.find_same id expenv) * typ'
      with
        Not_found -> typ'
      |	Solver.UnificationError report -> 
	  error_list (ident_locations id exp,
		 UnificationError (report, Report.Merge_context (Ident.name id)))

    end (Expr_context.remove id expenv)

  ) exp_expenv expenv



let abstract_context exp patenv expenv =

  Pat_context.fold (fun name (id, typ_pat) expenv ->

    try
      let typ_exp = Expr_context.find_same id expenv in
      typ_pat < typ_exp;
      Expr_context.remove id expenv
    with
	Not_found -> expenv
      |	Solver.UnificationError report ->
	  error_list (ident_locations id exp,
	      UnificationError (report, Report.Abstract_context (Ident.name id)))

  ) patenv expenv



(*-------------------------------------------------------------------------*)
(** {3 Sequences} *)

let map_fold_list f r list =
  List.fold_right (fun e (list, r) ->
    let e', r' = f r e in
    (e' :: list, r')
  ) list ([], r)

let type_sequence type_expr env map_fold pexps =

  let cset = mkcset () in
  let pc = level_variable cset in

  let exps, (row, expenv) =

    map_fold (fun (row, expenv) pexp ->
      let exp = type_expr env pexp in
      merge cset exp.exp_cset;
      pc < exp.exp_pc;
      row <& exp.exp_pc;
      exp, (row + exp.exp_row, 
	    merge_context exp exp.exp_context expenv)
    ) (row_variable cset, Expr_context.empty) pexps

  in

  cset, expenv, pc, exps, row


let rec type_sequence_pair env (pexp1, pexp2) =

  let map_fold f accu (pexp1, pexp2) =
    let exp1, accu' = f accu pexp1 in
    let exp2, accu'' = f accu' pexp2 in
    (exp1, exp2), accu''
  in

  type_sequence type_expr env map_fold (pexp1, pexp2)



and type_sequence_list env pexp_list =

  type_sequence type_expr env map_fold_list pexp_list



and type_sequence_1list env (pexp, pexp_list) =

  let map_fold f accu (pexp, pexp_list) =
    let exp_list, accu' = map_fold_list f accu pexp_list in
    let exp, accu'' = f accu' pexp in
    (exp, exp_list), accu''
  in

  type_sequence type_expr env map_fold (pexp, pexp_list)



and type_sequence_list2_option env (pexp_list, opt_pexp) =

  let map_fold f accu (pexp_list, opt_pexp) =
    let opt_exp, accu' = 
      match opt_pexp with
	None -> None, accu
      |	Some pexp ->
	  let exp, accu' = f accu pexp in
	  Some exp, accu'
    in
    let exp_list, accu'' = 
      map_fold_list (fun accu (lbl, pexp) -> 
	let exp, accu' = f accu pexp in
	(lbl, exp), accu'
      ) accu' pexp_list
    in
    (exp_list, opt_exp), accu''
  in

  type_sequence type_expr env map_fold (pexp_list, opt_pexp)



and type_option env opt_pexp =

  match opt_pexp with
    None -> 
      let cset = mkcset () in
      cset, Expr_context.empty, level_variable cset, None, row_variable cset
  | Some pexp ->
      let exp = type_expr env pexp in
      exp.exp_cset, exp.exp_context, exp.exp_pc, Some exp, exp.exp_row
	


(*-------------------------------------------------------------------------*)
(** {3 Simple expressions} *)

and type_expr env pexp =

  let loc = pexp.pexp_loc in

  match pexp.pexp_desc with

    Pexp_ident lid ->

      begin match Env.lookup_value loc lid env with

        (Pident id) as path, None ->

	  let cset = mkcset () in
          let typ = type_variable cset in
          { exp_desc = Texp_ident (path, None);
            exp_loc = loc;
            exp_cset = cset;
            exp_context = Expr_context.singleton id typ;
            exp_pc = level_variable cset;
            exp_typ = typ;
            exp_row = row_variable cset;
            exp_env = env
          } 

      | _, None -> assert false

      | path, Some vald ->

          let vald' = Value_description.copy vald in
          let cset = vald'.val_cset in

          { exp_desc = Texp_ident (path, Some vald);
            exp_loc = loc;
            exp_cset = cset;
            exp_context = vald'.val_context;
            exp_pc = level_variable cset;
            exp_typ = vald'.val_typ;
            exp_row = row_variable cset;
            exp_env = env
          } 

      end



  | Pexp_constant c ->

      let cset = mkcset () in

      { exp_desc = Texp_constant c;
        exp_loc = loc;
        exp_cset = cset;
        exp_context = Expr_context.empty;
        exp_pc = level_variable cset;
        exp_typ = begin
	  match c with
	    Const_int _ -> 
	      mktyp cset (Predef.int (level_variable cset))
	  | Const_char _ -> 
	      mktyp cset (Predef.char (level_variable cset))
	  | Const_string _ -> 
	      mktyp cset (Predef.string (level_variable cset))
	  | Const_float _ -> 
	      mktyp cset (Predef.float (level_variable cset))
	  | Const_charray _ -> 
	      mktyp cset 
		(Predef.charray (level_variable cset) (level_variable cset))
	end;
        exp_row = row_variable cset;
        exp_env = env
      } 



  | Pexp_let (rec_flag, ppat_pexp_list, pexp) -> 

      let letd = type_let env loc rec_flag ppat_pexp_list in

      let newenv =
	List.fold_left (fun env' (id, vald) ->
	  Env.add_value id (Some vald) env'
        ) env letd.let_gen
      in

      let exp = type_expr (Env.add_patenv letd.let_ng newenv) pexp in
      merge exp.exp_cset letd.let_cset;

      (* letd.let_pc < exp.exp_pc; *)
      letd.let_row <& exp.exp_pc;

      let expenv =
	merge_context exp
	  (abstract_context exp letd.let_ng exp.exp_context)
	  letd.let_context
      in

      { exp_desc = Texp_let (rec_flag, letd.let_desc, exp);
	exp_loc = loc;
	exp_cset = letd.let_cset;
	exp_context = expenv;
	exp_pc = letd.let_pc * exp.exp_pc;
	exp_typ = exp.exp_typ;
	exp_row = letd.let_row + exp.exp_row;
	exp_env = env
      }	



  | Pexp_function ppat_when_pexp_list ->

      let cset = mkcset () in
      
      let typ_arg = type_variable cset in
      let pc = level_variable cset in
      let pc' = level_variable cset in
      let row = row_variable cset in
      let row_when = row_variable cset in
      let typ_res = type_variable cset in
      pc < pc';
      row_when <& pc';
      
      let expenv, pat_when_exp_list =

	List.fold_left (fun (expenv, pat_when_exp_list) (ppat, opt_pexp, pexp) ->

          let pat = type_pattern false env ppat in
	  let newenv = Env.add_patenv pat.pat_context env in
          let exp = type_expr newenv pexp in

	  merge cset exp.exp_cset;
	  merge cset pat.pat_cset;

	  let opt_exp, expenv_when =
	    match opt_pexp with
	      None -> None, expenv
	    | Some pexp' ->
		let exp' = type_expr newenv pexp' in
		merge cset exp'.exp_cset;
		let expenv_when =
		  merge_context exp'
		    (abstract_context exp' pat.pat_context exp'.exp_context)
		    expenv
		in
		let lvl = level_variable cset in
		(exp'.exp_typ << mktyp cset (Predef.bool lvl))
		  exp'.exp_loc Report.ExprL;
		lvl < pc';
		lvl <| typ_res;
		pc' < exp'.exp_pc;
		exp'.exp_row < row_when;
		Some exp', expenv_when
	  in

	  let expenv' = 
	    merge_context exp
	      (abstract_context exp pat.pat_context exp.exp_context) 
	      expenv_when
	  in

          (exp.exp_typ << typ_res) exp.exp_loc Report.ExprL;
          exp.exp_row < row;
          (typ_arg << pat.pat_typ) pat.pat_loc Report.PatternR;
          pat.pat_level <| typ_res;
          pat.pat_level < pc';
          pc' < exp.exp_pc;

          (expenv', (pat, opt_exp, exp) :: pat_when_exp_list)

        ) (Expr_context.empty, []) ppat_when_pexp_list

      in

      { exp_desc = Texp_function (List.rev pat_when_exp_list, Total);
        exp_loc = loc;
        exp_cset = cset;
        exp_context = expenv;
        exp_pc = level_variable cset;
        exp_typ = mktyp cset 
	  (Predef.funct typ_arg pc row typ_res (level_variable cset));
        exp_row = row_when;
        exp_env = env
      } 



  | Pexp_apply (pexp_fun, pexp_args) ->

      let cset, expenv, pc, (exp_fun, exp_args), row = 
	type_sequence_1list env  (pexp_fun, pexp_args)
      in

      let _, typ_res, _, row_res =

        List.fold_left (fun (pc, typ_fun, loc, row) exp_arg ->

          let typ_res = type_variable cset in
          let lvl = level_variable cset in
          let pc' = pc + lvl in
          let row' = row_variable cset in
          row <& pc';
          lvl <| typ_res;

          (typ_fun << 
           mktyp cset (Predef.funct exp_arg.exp_typ pc' row' typ_res lvl))
            loc Report.ExprL;

          (pc', typ_res, Location.union loc exp_arg.exp_loc, row' + row)

        ) (pc, exp_fun.exp_typ, pexp_fun.pexp_loc, row) exp_args

      in

      { exp_desc = Texp_apply (exp_fun, exp_args);
        exp_loc = loc;
        exp_cset = cset;
        exp_context = expenv;
        exp_pc = pc;
        exp_typ = typ_res;
        exp_row = row_res;
        exp_env = env
      } 



  | Pexp_match (pexp_matched, ppat_when_pexp_list) ->

      let exp_matched = type_expr env pexp_matched in
      let cset = exp_matched.exp_cset in

      let pc' = level_variable cset in
      let row = row_variable cset in
      let typ = type_variable cset in
      exp_matched.exp_pc < pc';
      exp_matched.exp_row <& pc';

      let row_when = row_variable cset in
      row_when < row;
      row_when <& pc';

      let expenv, pat_when_exp_list =

        List.fold_left (fun (expenv, pat_when_exp_list) (ppat, opt_pexp, pexp) ->

          let pat = type_pattern false env ppat in
	  let newenv = Env.add_patenv pat.pat_context env in
          let exp = type_expr newenv pexp in

          merge cset exp.exp_cset;
          merge cset pat.pat_cset;

	  let opt_exp, expenv_when =
	    match opt_pexp with
	      None -> None, expenv
	    | Some pexp' ->
		let exp' = type_expr newenv pexp' in
		merge cset exp'.exp_cset;
		let expenv_when =
		  merge_context exp'
		    (abstract_context exp' pat.pat_context exp'.exp_context)
		    expenv
		in
		let lvl = level_variable cset in
		(exp'.exp_typ << mktyp cset (Predef.bool lvl))
		  exp'.exp_loc Report.ExprL;
		lvl < pc';
		lvl <| typ;
		pc' < exp'.exp_pc;
		exp'.exp_row < row_when;
		Some exp', expenv_when
	  in

	  let expenv' = 
	    merge_context exp
	      (abstract_context exp pat.pat_context exp.exp_context) 
	      expenv_when
	  in

          (exp_matched.exp_typ << pat.pat_typ) ppat.ppat_loc Report.PatternR;
	  pat.pat_level < pc';
	  pc' < exp.exp_pc;
	  (exp.exp_typ << typ) pexp.pexp_loc Report.ExprL;
          pat.pat_level <| typ;
	  exp.exp_row < row;

          (expenv', (pat, opt_exp, exp) :: pat_when_exp_list)

        ) (exp_matched.exp_context, []) ppat_when_pexp_list

      in

      { exp_desc = Texp_match (exp_matched, List.rev pat_when_exp_list, Total);
        exp_loc = loc;
        exp_cset = cset;
        exp_context = expenv;
        exp_pc = exp_matched.exp_pc;
        exp_typ = typ;
        exp_row = row;
        exp_env = env
      } 



  | Pexp_raise (lid, opt_pexp) ->

      let path, exn = Env.lookup_exception loc lid env in
      let exn' = Exception_declaration.copy exn in
      let cset = exn'.exn_cset in

      let cset, expenv, pc, opt_exp, row = type_option env opt_pexp in
      merge cset exn'.exn_cset;

      begin match opt_exp with
	None -> ()
      |	Some exp ->
	  (exp.exp_typ << tuplify_argument cset exn'.exn_args)
	    exp.exp_loc Report.ExprL
      end;

      let pc' =
        match exn'.exn_param with
          None -> pc
        | Some pc0 -> pc + pc0
      in

      { exp_desc = Texp_raise (exn', opt_exp);
        exp_loc = loc;
        exp_cset = cset;
        exp_context = expenv;
        exp_pc = pc;
        exp_typ = type_variable cset;
        exp_row = row + (Solver.row cset (path, pc', row_variable cset));
        exp_env = env
      } 



  | Pexp_try (pexp0, ptry_pexp_list) ->

      let exp0 = type_expr env pexp0 in
      let cset = exp0.exp_cset in

      let typ = type_variable cset in
      let row = row_variable cset in

      exp0.exp_typ < typ;

      let expenv, row_remainder, try_exp_list =

        List.fold_left (fun
	  (expenv, row_remainder, try_exp_list)
            (ptry, pexp, try_case) ->

          let ttry = type_try_pattern env ptry in
          let exp = type_expr (Env.add_patenv ttry.try_context env) pexp in

          merge cset exp.exp_cset;
          merge cset ttry.try_cset;

          let expenv' =
            merge_context exp
	      (abstract_context exp ttry.try_context exp.exp_context)
	      expenv
          in

          row_remainder < ttry.try_handled;
          exp0.exp_pc < exp.exp_pc;
          ttry.try_level < exp.exp_pc;

          begin match try_case with
            Propagate _ ->
              ttry.try_reraise < row
          | Throw ->
              (exp.exp_typ << typ) pexp.pexp_loc Report.ExprL;
              ttry.try_level <| typ;
	      exp.exp_row < row
	  end;

          (expenv', ttry.try_remainder, (ttry, exp, try_case) :: try_exp_list)

        ) (exp0.exp_context, exp0.exp_row, [])
          ptry_pexp_list

      in

      row_remainder < row;

      { exp_desc = Texp_try (exp0, List.rev try_exp_list);
        exp_loc = loc;
        exp_cset = cset;
        exp_context = expenv;
        exp_pc = exp0.exp_pc;
        exp_typ = typ;
        exp_row = row;
        exp_env = env
      } 



  | Pexp_finally (pexp1, pexp2) ->

      let exp1 = type_expr env pexp1
      and exp2 = type_expr env pexp2 in

      let cset = exp1.exp_cset in
      merge cset exp2.exp_cset;

      { exp_desc = Texp_finally (exp1, exp2);
        exp_loc = loc;
        exp_cset = cset;
        exp_context = 
  	  merge_context exp2 exp2.exp_context exp1.exp_context;
        exp_pc = exp1.exp_pc * exp2.exp_pc;
        exp_typ = exp1.exp_typ;
        exp_row = exp1.exp_row;
        exp_env = env
      } 



  | Pexp_tuple pexp_list ->

      let cset, expenv, pc, exp_list, row = type_sequence_list env pexp_list in

      { exp_desc = Texp_tuple exp_list;
        exp_loc = loc;
        exp_cset = cset;
        exp_context = expenv;
        exp_pc = pc;
        exp_typ = 
	  mktyp cset (Predef.tuple (List.map (fun exp -> exp.exp_typ) exp_list));
        exp_row = row;
        exp_env = env
      } 



  | Pexp_construct (lid, opt_pexp, _) ->

      let cstr = 
	Constructor_description.copy (Env.lookup_constructor loc lid env)
      in
      check_cstr_expr loc lid cstr opt_pexp; 

      let cset, expenv, pc, opt_exp, row = type_option env opt_pexp in
      merge cset cstr.cstr_cset;

      begin match opt_exp with
	None -> ()
      |	Some exp ->
          (exp.exp_typ << tuplify_argument cset cstr.cstr_args)
	    exp.exp_loc Report.ExprL
      end;

      { exp_desc = Texp_construct (cstr, opt_exp);
        exp_loc = loc;
        exp_cset = cset;
        exp_context = expenv;
        exp_pc = pc;
        exp_typ = cstr.cstr_res;
        exp_row = row;
        exp_env = env
      } 



  | Pexp_record (lid_pexp_list, opt_pexp) ->

      let lbl_pexp_list =
	List.sort (fun (lbl1, _) (lbl2, _) ->
	  Pervasives.compare lbl1.lbl_pos lbl2.lbl_pos
        )
	(List.map (function lid, pexp ->
	   let lbl = Label_description.copy (Env.lookup_label loc lid env) in
	   lbl, pexp
        ) lid_pexp_list)
      in

      let cset, expenv, pc, (lbl_exp_list, opt_exp), row = 
	type_sequence_list2_option env (lbl_pexp_list, opt_pexp)
      in

      let typ = type_variable cset in

      List.iter (function lbl, exp ->

        merge cset lbl.lbl_cset;
        (exp.exp_typ << lbl.lbl_arg) exp.exp_loc Report.ExprL;
        (lbl.lbl_res << typ) loc Report.ExprR

      ) lbl_exp_list;

      check_record (opt_pexp = None) loc lbl_pexp_list; 

      begin match opt_exp with
        None -> ()
      | Some exp -> (exp.exp_typ << typ) exp.exp_loc Report.ExprL
      end;

      { exp_desc = Texp_record (lbl_exp_list, opt_exp);
        exp_loc = loc;
        exp_cset = cset;
        exp_context = expenv;
        exp_pc = pc;
        exp_typ = typ;
        exp_row = row;
        exp_env = env
      } 



  | Pexp_field (pexp, lid) ->

      let lbl = Label_description.copy (Env.lookup_label loc lid env) in
      let exp = type_expr env pexp in
      merge exp.exp_cset lbl.lbl_cset;

      (exp.exp_typ << lbl.lbl_res) exp.exp_loc Report.ExprL;

      let typ = type_variable exp.exp_cset in
      lbl.lbl_arg < typ;
      begin match lbl.lbl_level with
        None -> ()
      | Some lvl0 -> lvl0 <| typ
      end;

      { exp_desc = Texp_field (exp, lbl);
        exp_loc = loc;
        exp_cset = exp.exp_cset;
        exp_context = exp.exp_context;
        exp_pc = exp.exp_pc;
        exp_typ = typ;
        exp_row = exp.exp_row;
        exp_env = env
      } 



  | Pexp_setfield (pexp1, lid, pexp2) ->

      let lbl = Label_description.copy (Env.lookup_label loc lid env) in
      if lbl.lbl_mut = Immutable then error (loc, Label_not_mutable lid);

      let cset, expenv, pc, (exp1, exp2), row = 
	type_sequence_pair env (pexp1, pexp2)
      in
      merge cset lbl.lbl_cset;

      (exp1.exp_typ << lbl.lbl_res) exp1.exp_loc Report.ExprL;
      (exp2.exp_typ << lbl.lbl_arg) exp2.exp_loc Report.ExprL;

      pc <| lbl.lbl_arg;
      pc < exp1.exp_pc;

      begin match lbl.lbl_level with
        None -> ()
      | Some lvl -> lvl <| lbl.lbl_arg
      end;

      { exp_desc = Texp_setfield (exp1, lbl, exp2);
        exp_loc = loc;
        exp_cset = cset;
        exp_context = expenv;
        exp_pc = pc;
        exp_typ = mktyp cset Predef.unit;
        exp_row = row;
        exp_env = env
      } 



  | Pexp_array pexp_list ->

      let cset, expenv, pc, exp_list, row = type_sequence_list env pexp_list in

      let typ = type_variable cset in

      List.iter (function exp ->
	(exp.exp_typ << typ) exp.exp_loc Report.ExprL
      ) exp_list;

      { exp_desc = Texp_array exp_list;
        exp_loc = loc;
        exp_cset = cset;
        exp_context = expenv;
        exp_pc = pc;
        exp_typ = mktyp cset (Predef.array typ (level_variable cset));
        exp_row = row;
        exp_env = env
      } 
      


  | Pexp_ifthenelse (pexp, pexp1, opt_pexp2) ->

      let exp = type_expr env pexp in
      let cset = exp.exp_cset in
      
      let lvl = level_variable cset in
      (exp.exp_typ << mktyp cset (Predef.bool lvl)) exp.exp_loc Report.ExprL;
      
      let exp1 = type_expr env pexp1 in
      merge cset exp1.exp_cset;
      lvl < exp1.exp_pc;
      exp.exp_pc < exp1.exp_pc;
      exp1.exp_row < exp.exp_row;
      
      let expenv1 = 
	merge_context exp1 exp1.exp_context exp.exp_context
      in

      let typ = type_variable cset in
      exp1.exp_typ < typ;

      let opt_exp2, expenv2 =
	match opt_pexp2 with
	  None -> 
	    ((mktyp cset Predef.unit) << typ)
	      exp1.exp_loc Report.ExprR;
	    None, expenv1
	| Some pexp2 ->
	    let exp2 = type_expr env pexp2 in
	    merge cset exp2.exp_cset;
	    lvl < exp2.exp_pc;
	    exp.exp_pc < exp2.exp_pc;
	    exp2.exp_row < exp.exp_row;
	    (exp2.exp_typ << typ) exp2.exp_loc Report.ExprL;
	    lvl <| typ;
	    (Some exp2),
	    merge_context exp2 exp2.exp_context expenv1
      in

      { exp_desc = Texp_ifthenelse (exp, exp1, opt_exp2);
        exp_loc = loc;
        exp_cset = cset;
        exp_context = expenv2;
        exp_pc = exp.exp_pc;
        exp_typ = typ;
        exp_row = exp.exp_row;
        exp_env = env
      } 



  | Pexp_sequence (pexp1, pexp2) ->

      let cset, expenv, pc, (exp1, exp2), row = 
	type_sequence_pair env (pexp1, pexp2)
      in

      { exp_desc = Texp_sequence (exp1, exp2);
        exp_loc = loc;
        exp_cset = cset;
        exp_context = expenv;
        exp_pc = pc;
        exp_typ = exp2.exp_typ;
        exp_row = row;
        exp_env = env
      } 



  | Pexp_while (pexp1, pexp2) ->

      let exp1 = type_expr env pexp1
      and exp2 = type_expr env pexp2 in
      let cset = exp1.exp_cset in
      merge cset exp2.exp_cset;

      let lvl = level_variable cset in
      let pc = level_variable cset in

      (exp1.exp_typ << mktyp cset (Predef.bool lvl)) exp1.exp_loc Report.ExprL;
      lvl < exp1.exp_pc; pc < exp1.exp_pc; 
      lvl < exp2.exp_pc; pc < exp2.exp_pc;
      exp1.exp_row <& exp1.exp_pc;
      exp2.exp_row <& exp2.exp_pc;

      let expenv =
	merge_context exp1 exp1.exp_context exp2.exp_context
      in
      
      { exp_desc = Texp_while (exp1, exp2);
        exp_loc = loc;
        exp_cset = cset;
        exp_context = expenv;
        exp_pc = pc;
        exp_typ = mktyp cset Predef.unit;
        exp_row = exp1.exp_row + exp2.exp_row;
        exp_env = env
      } 



  | Pexp_for (name, pexp_lo, pexp_hi, direction_flag, pexp) ->

      let cset, expenv, pc, (exp_lo, exp_hi), row =
	type_sequence_pair env (pexp_lo, pexp_hi)
      in

      let id = Ident.create name in
      let lvl = level_variable cset in
      let int_lvl = mktyp cset (Predef.int lvl) in

      (exp_lo.exp_typ << int_lvl) exp_lo.exp_loc Report.ExprL;
      (exp_hi.exp_typ << int_lvl) exp_hi.exp_loc Report.ExprL;

      let patenv = Pat_context.singleton name (id, int_lvl) in
      let exp = type_expr (Env.add_patenv patenv env) pexp in
      merge cset exp.exp_cset;

      let expenv' = 
	merge_context exp
	  (abstract_context exp patenv exp.exp_context)
	  expenv
      in

      pc < exp.exp_pc;
      lvl < exp.exp_pc;
      exp.exp_row <& exp.exp_pc;

      { exp_desc = Texp_for (id, exp_lo, exp_hi, direction_flag, exp);
        exp_loc = loc;
        exp_cset = cset;
        exp_context = expenv';
        exp_pc = pc;
        exp_typ = mktyp cset Predef.unit;
        exp_row = row + exp.exp_row;
        exp_env = env
      }       



  | Pexp_constraint (pexp, ptys) ->

      let exp = type_expr env pexp in
      let cset, typ = Transl_typeexpr.transl_scheme env ptys in
      merge exp.exp_cset cset;

      (exp.exp_typ << typ) exp.exp_loc Report.ExprL;
      { exp with exp_typ = typ }



  | Pexp_assert pexp ->

      let exp = type_expr env pexp in
      let cset = exp.exp_cset in

      (exp.exp_typ << mktyp cset (Predef.bool (level_variable cset)))
	exp.exp_loc Report.ExprL;

      { exp with exp_typ = mktyp cset Predef.unit }
  


  | Pexp_assertfalse ->

      let cset = mkcset () in

      { exp_desc = Texp_assertfalse;
        exp_loc = loc;
        exp_cset = cset;
        exp_context = Expr_context.empty;
        exp_pc = level_variable cset;
        exp_typ = type_variable cset;
        exp_row = row_variable cset;
        exp_env = env
      } 



  | Pexp_paren pexp ->
      type_expr env pexp



(*-------------------------------------------------------------------------*)
(** {3 Let-definitions} *)

and type_let env loc rec_flag ppat_pexp_list =

  if rec_flag = Recursive then check_letrec ppat_pexp_list;

  (* Typing patterns *)
  let pat_pexp_list =
    List.map (function ppat, pexp ->
      let pat = type_pattern false env ppat in
      (* Correction faite le 16/05/16 - Flow Caml 1.06 *)
      (* Un peu ad'hoc, c'est une règle différente de si on avait un match
         à un cas... *)
      Pat_context.iter (function typ ->
        pat.pat_level <| typ;
      ) pat.pat_context;
      (* Fin de la correction *)
      pat, pexp
    ) ppat_pexp_list
  in

  let cset, patenv, _ = pattern_product loc (List.map fst pat_pexp_list) in

  let env' =
    match rec_flag with
      Nonrecursive | Default -> env
    | Recursive -> Env.add_patenv patenv env
  in

  (* Typing expressions *)

  let pc = level_variable cset in

  let row, expenv, pat_exp_list =
    List.fold_right (fun (pat, pexp) (row, expenv, pat_exp_list) ->

      let exp = type_expr env' pexp in
      merge cset exp.exp_cset;

      pc < exp.exp_pc;
      row <& exp.exp_pc;
      (exp.exp_typ << pat.pat_typ) pexp.pexp_loc Report.ExprL;

      let expenv' =
	merge_context exp begin
	  match rec_flag with
	    Nonrecursive | Default -> exp.exp_context
	  | Recursive -> abstract_context exp patenv exp.exp_context
	end expenv
      in
      (row + exp.exp_row,
       expenv',
       (pat, exp) :: pat_exp_list)

    ) pat_pexp_list (row_variable cset, Expr_context.empty, [])
  in

  (* Solving *)

  let let_def =
    { let_desc = pat_exp_list;
      let_cset = cset;
      let_gen = [];
      let_ng = patenv;
      let_context = expenv;
      let_pc = pc;
      let_row = row;
      let_env = env
    } 
  in

  (* Non-generalizable definitions *)

  let ng, gen_expenv, has_generalizable = 
    List.fold_left (fun (ng, gen_expenv, has_generalizable) (pat, exp) ->
      if (generalizable exp) then (ng, gen_expenv, true) else begin
	Pat_context.fold (fun name (id, typ) 
	    (ng, gen_expenv, has_generalizable) ->
	  Pat_context.add name (id, typ) ng,
	  Expr_context.add id typ gen_expenv,
	  has_generalizable
        ) pat.pat_context (ng, gen_expenv, has_generalizable)
      end
    ) (Pat_context.empty, expenv, false) pat_exp_list
  in

  (* Solving *)

  if has_generalizable then begin
    match LetDefinition.solve let_def with
      None -> ()
    | Some report -> error (loc, SolveLet report)
  end;

  (* Generalizable definitions *)

  let gen =
    List.fold_left (fun gen (pat, exp) ->
      if not (generalizable exp) then gen else begin
	Pat_context.fold (fun name (id, typ) gen ->
	  let vald = Value_description.copy
	      { val_cset = cset;
		val_context = gen_expenv;
		val_typ = typ }
	  in
	  ignore (Value_description.solve vald);
	  (id, vald) :: gen
        ) pat.pat_context gen
      end
    ) [] pat_exp_list
  in

  { let_desc = pat_exp_list;
    let_cset = cset;
    let_gen = gen;
    let_ng = ng;
    let_context = expenv;
    let_pc = pc;
    let_row = row;
    let_env = env
  } 




(***************************************************************************)
(** {2 Typing top-level phrases} *)

let type_let_toplevel env loc rec_flag ppat_pexp_list =

  let letd = type_let env loc rec_flag ppat_pexp_list in

  begin match LetDefinition.solve letd with
    None -> ()
  | Some report -> error (loc, SolveLet report)
  end;

  let definitions =
    Pat_context.fold (fun name (id, typ) accu ->
      let vald = Value_description.copy
	  { val_cset = letd.let_cset;
	    val_context = Expr_context.empty;
	    val_typ = typ }
      in
      ignore (Value_description.solve vald);
      begin match Value_description.has_minimal_instance vald with
	None -> ()
      |	Some report -> error (loc, Non_generalizable (id, report))
      end;
      (id, vald) :: accu
    ) letd.let_ng letd.let_gen
  in

  let signature, newenv = 
    List.fold_left (fun (signature, newenv) (id, vald) ->
      Tsig_value (id, vald) :: signature,
      Env.add_value id (Some vald) newenv
    ) ([], env) definitions
  in

  ignore (LetDefinition.solve letd);
  (* TEMPORARY On peut optimiser dans le cas ou on n'a 
     que des trucs generalisables: il est inutile de resoudre ce que l'on jette
   *)

  letd.let_desc,
  signature,
  newenv,
  Solver.get_upper_bound letd.let_pc,
  Solver.get_lower_bound letd.let_row



let type_eval env loc pexp =

  let exp = type_expr env pexp in

  begin match Expression.solve exp with
    None -> ()
  | Some report -> error (loc, SolveExpr report)
  end;

  exp, 
  Solver.get_upper_bound exp.exp_pc,
  Solver.get_lower_bound exp.exp_row
