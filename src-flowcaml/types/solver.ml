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

(* $Id: solver.ml,v 1.12 2003/09/23 14:46:40 simonet Exp $ *)
(* Solver: The Dalton instance. *)

open Format
open Dalton_aux
open Type_constructor
open Types



type 'a typ =
    { constr: Type_constructor.t;
      sons: 'a list
    } 



(***************************************************************************)

module Lb = struct

  type t = Level.Set.t

  let bottom = Level.Set.empty
  let is_bottom = Level.Set.is_empty
  let union = Level.Set.union

  let leq set1 set2 = 
    Level.Set.for_all (fun level1 ->
      Level.Set.exists (fun level2 ->
	Env.level_leq level1 level2 
      ) set2
    ) set1

  let leq_report set1 set2 = 
    Level.Set.choose (Level.Set.filter (fun level1 ->
      not (Level.Set.exists (fun level2 -> Env.level_leq level1 level2) set2)
    ) set1)


  let compare = Level.Set.compare

  let normalize set =
    Level.Set.fold (fun level set' ->
      if Level.Set.exists (fun level' -> Env.level_leq level level') set'
      then set'
      else
        Level.Set.add level (Level.Set.filter (fun level' ->
          not (Env.level_leq level' level)
        ) set')
    ) set Level.Set.empty

  let fprint = Level.Set.fprint

  let rec fprint_in_term_aux simplify i ppf lb =
    match i with
      0 ->
	if simplify && Level.Set.cardinal lb = 1 
	then Level.Set.fprint ppf lb
        else Format.fprintf ppf "[> %a]" Level.Set.fprint lb
    | 1 -> Format.fprintf ppf "*: %a" (fprint_in_term_aux simplify 0) lb
    | _ -> assert false

  let fprint_in_term = fprint_in_term_aux true
      
  let draw set = 
      List.map Level.to_string (Level.Set.elements set)

end



module Ub = struct

  type t = Level.Set.t

  let top = Level.Set.empty
  let is_top = Level.Set.is_empty
  let inter = Level.Set.union

  let geq set1 set2 =
    Level.Set.for_all (fun level1 ->
      Level.Set.exists (fun level2 -> Env.level_leq level2 level1) set2
    ) set1

  let geq_report set1 set2 =
    Level.Set.choose (Level.Set.filter (fun level1 ->
      not (Level.Set.exists (fun level2 -> Env.level_leq level2 level1) set2)
    ) set1)

  let compare = Level.Set.compare

  let normalize set =
    Level.Set.fold (fun level set' ->
      if Level.Set.exists (fun level' -> Env.level_leq level' level) set'
      then set'
      else
        Level.Set.add level (Level.Set.filter (fun level' ->
          not (Env.level_leq level level')
        ) set')
    ) set Level.Set.empty

  let fprint = Level.Set.fprint

  let rec fprint_in_term_aux simplify i ppf ub =
    match i with
      0 ->
        if simplify && Level.Set.cardinal ub = 1
        then Level.Set.fprint ppf ub
        else Format.fprintf ppf "[< %a]" Level.Set.fprint ub
    | 1 -> Format.fprintf ppf "*: %a" (fprint_in_term_aux simplify 0) ub
    | _ -> assert false

  let fprint_in_term = fprint_in_term_aux true
      
  let draw = Lb.draw

end



module Lub = struct

  let leq lb_set ub_set =
    Level.Set.for_all (function level1 ->
      Level.Set.for_all (function level2 ->
        Env.level_leq level1 level2
      ) ub_set
    ) lb_set

  let leq_report lb_set ub_set =
    match
      Level.Set.fold (fun level1 accu ->
	Level.Set.fold (fun level2 accu ->
          if Env.level_leq level1 level2 then accu else (level1, level2) :: accu
        ) ub_set accu
      ) lb_set []
    with
      [] -> raise Not_found
    | hd :: _ -> hd

  let geq lb_set ub_set =
    Level.Set.exists (function level1 ->
      Level.Set.exists (function level2 ->
        Env.level_leq level2 level1
      ) ub_set
    ) lb_set

  let rec fprint_in_term i ppf lb_set ub_set =
    match i with
      0 ->
	begin match Level.Set.is_empty lb_set, Level.Set.is_empty ub_set with
	  true, true -> assert false
	| true, false -> Ub.fprint_in_term_aux false i ppf ub_set
	| false, true -> Lb.fprint_in_term_aux false i ppf lb_set
	| false, false -> (* TEMPORARY *)
	    begin try
	      Level.Set.iter (function level1 ->
		Level.Set.iter (function level2 ->
		  if Env.level_leq level2 level1 then
		    (Level.fprint ppf level1; raise Exit)
                ) ub_set
              ) lb_set;
	      fprintf ppf "[> %a |< %a]" 
		Level.Set.fprint lb_set
		Level.Set.fprint ub_set
	    with
	      Exit -> ()
	    end
	end
    | 1 -> 
	Format.fprintf ppf "*: ";
	fprint_in_term 0 ppf lb_set ub_set
    | _ -> assert false

  let fprint_notleq ppf lb_set ub_set =
    ignore begin
      Level.Set.fold (fun level1 first ->
	Level.Set.fold (fun level2 first ->
	  if not (Env.level_leq level1 level2) then begin
	    if not first then fprintf ppf "@ ";
	    fprintf ppf "%a < %a" Level.fprint level1 Level.fprint level2;
	    false
	  end else first
        ) ub_set first
      ) lb_set true
    end
    

end




(***************************************************************************)

module Ground = struct

  module Lb = Lb
  module Ub = Ub
  module Lub = Lub



  (*-----------------------------------------------------------------------*)

  module Label = struct

    type t = Path.t
    let compare = Path.compare
    let hash = Path.hash
    let fprint = Path.fprint

  end



  (*-----------------------------------------------------------------------*)

  module Type = struct

    type 'a t = 'a typ

    let ldestr_inv = true
    let rdestr_inv = false

    let ldestr t =
      not t.constr.tc_fun

    let rdestr t =
      true

    let compatible t1 t2 =
      match t1.constr.tc_desc, t2.constr.tc_desc with
        TCfunction, TCfunction -> true
      | TCtuple i1, TCtuple i2 -> i1 = i2
      | TCpath path1, TCpath path2 -> Path.same path1 path2
      | _ -> false

    let map f t =
      { t with sons = List.map2 f (convert t.constr) t.sons }

    let iter f t =
      List.iter2 f (convert t.constr) t.sons

    let iter2 f t1 t2 =
      assert (same t1.constr t2.constr);
      Standard.iter3 f (convert t1.constr) t1.sons t2.sons

    let for_all2 f t1 t2 =
      same t1.constr t2.constr
        &&
      Standard.for_all3 f (convert t1.constr) t1.sons t2.sons

    let map2 f t1 t2 =
      { t1 with sons = Standard.map3 f (convert t1.constr) t1.sons t2.sons }

    let hash t =
      Hashtbl.hash t


    (* Pretty print *)

    type position =
        Dumb
      | FunctionArgument
      | FunctionResult
      | TupleElement
      | UnaryConstructorArgument
      | ConstructorArgument

    let parenthesize position t =

      match t.constr.tc_desc, position with

        _, Dumb -> assert false
      | _, ConstructorArgument -> false

      | TCfunction, FunctionResult -> false
      | TCfunction,
          (FunctionArgument | TupleElement | UnaryConstructorArgument) -> true

      | TCtuple _, (FunctionArgument | FunctionResult) -> false
      | TCtuple _, (TupleElement | UnaryConstructorArgument) -> true

      | TCpath _, _ -> false


    let rec fprint_list sep f ppf = function
        [] -> ()
      | hd :: [] -> fprintf ppf "%a" f hd
      | hd :: tl -> fprintf ppf "%a%s@ %a" f hd sep (fprint_list sep f) tl

    let arg v = (* TEMPORARY *)
      { variance = v;
        kind = Katom;
        ldestr = false;
        rdestr = false
      }         

    let fprint ppf skip f t =
      
      match t.constr.tc_desc, t.sons, List.map fst t.constr.tc_prop with
        
        TCfunction, [t1; t2; t3; t4; t5], _ -> 
          begin match skip t2, skip t3, skip t5 with
            true, true, true ->
              fprintf ppf "@[%a ->@ %a@]" 
                (f (arg Contravariant) FunctionArgument) t1
                (f (arg Covariant) FunctionResult) t4

          |  skip2, skip3, skip5 ->     
              fprintf ppf "@[%a -{" (f (arg Contravariant) FunctionArgument) t1;
              if skip2 then fprintf ppf "|"
              else fprintf ppf "%a |" (f (arg Contravariant) Dumb) t2;
              if skip3 then fprintf ppf "|" else 
              fprintf ppf " @[<hov>%a@] |" (f (arg Covariant) Dumb) t3;
              if not skip5 then 
                fprintf ppf " %a" (f (arg Covariant) Dumb) t5;
              fprintf ppf "}->@ %a@]" (f (arg Covariant) FunctionResult) t4
              
          end

      | TCtuple _, list, _ ->
          fprintf ppf "@[<0>%a@]"
            (fprint_list " *" (f (arg Covariant) TupleElement)) list

      | TCpath p, [], [] ->
          Path.fprint ppf p

      | TCpath p, [ hd ], [ v ] ->
          fprintf ppf "@[%a@ %a@]"
            (f (arg v) UnaryConstructorArgument) hd  Path.fprint p

      | TCpath p, list, variances  ->
          let rec aux ppf = function
              [], [] -> ()
            | [t], [v] -> f (arg v) ConstructorArgument ppf t
            | t :: list', v :: variances' ->
                fprintf ppf "%a,@ %a"
                  (f (arg v) ConstructorArgument) t
                  aux (list', variances')
            | _ -> assert false
          in
          fprintf ppf "@[@[<1>(%a)@]@ %a@]"
            aux (list, variances)
            Path.fprint p

      | _ -> assert false

  end

end



(***************************************************************************)

module Print = struct

  include Dalton_templates.Print

  let left_destructor printer ppf x =
    fprintf ppf "content(%a)" printer x

  let right_destructor printer ppf x =
    fprintf ppf "level(%a)" printer x

  let left_destructor_skel printer ppf x =
    fprintf ppf "content~(%a)" printer x

  let right_destructor_skel printer ppf x =
    fprintf ppf "level~(%a)" printer x

end



module Report = struct

  include Dalton_templates.ErrorReport

  type unification_message =
      ExprL | ExprR
    | PatternL | PatternR
    | Or_pattern
    | Or_pattern_var of string
    | Pattern_for of string
    | Merge_context of string
    | Abstract_context of string

  let unification_message : unification_message ref = 
    ref ExprL

  let unification ppf ~term1 ~term2 ~explanation =
    let term1, term2 =
      match !unification_message with
        ExprR | PatternR -> term2, term1
      | _ -> term1, term2
    in

    let msg1, msg2 =
      match !unification_message with
        ExprL | ExprR ->
          "This expression has type",
          "but is here used with type"
      | PatternL | PatternR -> 
          "This pattern matches values of type",
          "but is here used to match values of type"
      | Or_pattern -> 
          "The left hand side of this or-pattern matches values of type",
          "but the right hand side matches values of type"
      | Or_pattern_var name -> 
          "The identifier " ^ name ^ 
          " is bound in the left hand side of this pattern to a value of type",
          "but in the right hand side to a value of type"
      | Abstract_context name ->
          "The identifier " ^ name ^ " is bound to a value of type",
          "but is here used with type"
      | Pattern_for name ->
          "This loop binds the indenfifier " ^ name ^ " to a value of type",
          "but it is used inside the loop with type"
      | Merge_context name ->
          "The identifier " ^ name ^ " has type",
          "but is here used with type"
    in

    fprintf ppf
      "@[<v>@[%s@;<1 2>%t@ %s@;<1 2>%t@ %t@]@]"
      msg1 term1 msg2 term2 explanation

  let cycle ppf ~variable ~term =
    fprintf ppf
      "@[<v>@[The solutions of the equation@;<1 2>%t = %t@;<1 2>\
      are recursive."
      variable term

  let incompatible ppf ~term1 ~term2 =
    fprintf ppf
      "@[<v>@[The type@;<1 2>%t@ is not compatible with@;<1 2>%t@]@]"
      term1 term2

  let minimal ppf ~scheme ~variables =
    fprintf ppf
      "@;<1 2>%t@ cannot be generalized because the following variables have \
      no minimal instance:@;<1 2>%t"
      scheme variables


  let ldestr ppf ~term =
    fprintf ppf
      "@[<v>@[Generic primitives cannot be \
      applied on expressions of type@;<1 2>%t@]@]" term

  let inequality ppf ~explanation =
    fprintf ppf
      "@[<v>This expression generates the following information flow:@ \
      @[<v 2>  %t@]@ which is not legal.@]" explanation

end

include Dalton.Make
    (Ground)
    (Print)
    (Draw)
    (Report)
