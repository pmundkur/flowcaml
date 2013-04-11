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

(* $Id: fkind.ml,v 1.2 2003/06/26 13:32:56 simonet Exp $ *)
(* Fkind: kinds *)

open Format
open Dalton_aux
module Proxy = Dalton_lib.Proxy



type t =
    Level
  | Type
  | Row of Path.t list (* TEMPORARY : Remplacer par un ensemble ? *)



let fprint ppf = function
    Level -> fprintf ppf "level"
  | Type -> fprintf ppf "type"
  | Row list -> 
      fprintf ppf "row[@[%a@]]" 
	(Misc.fprint_list (fun ppf -> Format.fprintf ppf ";@ ") Path.fprint) list



let equal kind1 kind2 =
  match kind1, kind2 with
    Level, Level | Type, Type -> true
  | Row list1, Row list2 ->
      if List.length list1 <> List.length list2 then false else 
      List.for_all2 Path.same
	(List.sort Path.compare list1) (List.sort Path.compare list2)
  | _ -> false

let daltonize = function
    Level -> Katom
  | Type -> Ktype
  | Row _ -> Krow Katom



(* -------------------------------------------------------------------------
   UNIFICATION OVER SET OF ROW-LABELS

   The module [Uset] implements a representation of row-label sets with
   variables and unification.
 *)

module Cset = Set.Make (struct
  type t = Path.t
  let compare = Path.compare
end)



module Uset = struct

  type desc =
    Var of Cset.t
      (* A set variable included in the complementary of its argument :) *)
  | Empty
      (* The empty set. *)
  | Element of Path.t * t  
      (* Disjoint union of a singleton and an other set. *)

  and node = { mutable desc: desc }

  and t = node Proxy.t



  (* [create] generates a new set with the given descriptor. *)

  let create desc =
    Proxy.create { desc = desc }



  (* [forbidden] *)

  exception Forbidden

  let rec forbidden forb set =

    let setn = Proxy.desc set in

    match setn.desc with

      Var forb' -> 
	setn.desc <- Var (Cset.union forb forb')

    | Empty -> ()

    | Element (lbl, set') ->
	if Cset.mem lbl forb then raise Forbidden;
	forbidden forb set'



  (* [unify] unifies two sets. *)

  exception Unify

  let rec unify set1 set2 =

    let set1n = Proxy.desc set1
    and set2n = Proxy.desc set2 in
   
    begin match set1n.desc, set2n.desc with

      _, Var forb2 ->
	forbidden forb2 set1

    | Var forb1, _ ->
	forbidden forb1 set2;
	set1n.desc <- set2n.desc

    | Empty, Empty ->
	()

    | Empty, Element _ | Element _, Empty -> raise Unify

    | Element (lbl1, set1'), Element (lbl2, set2') ->
	if Path.compare lbl1 lbl2 <> 0 then begin
	  let set'' = create
	      (Var (Cset.union (Cset.singleton lbl1) (Cset.singleton lbl2)))
	  in
	  unify set1' (create (Element (lbl2, set'')));
	  unify set2' (create (Element (lbl1, set'')))
	end
	else unify set1' set2'

    end;

    Proxy.linksto set2 set1



  (* [freeze] returns the list represented by a set.  It raises [Freeze]
     if the given set contains a free variable. 
     TEMPORARY Rendre tail-recursive ? *)

  exception Freeze

  let rec freeze set =
    match (Proxy.desc set).desc with
      Var _ -> raise Freeze
    | Empty -> []
    | Element (x, set') -> x :: (freeze set')



  (* [unfreeze] returns a constant set represented by a list. *)

  let rec unfreeze list = 
    List.fold_left (fun accu lbl ->
      create (Element (lbl, accu))
    ) (create Empty) list



  (* Pretty-printing : [fprint] *)

  let rec fprint ppf set =
    match (Proxy.desc set).desc with
      Var _ -> fprintf ppf "?"
    | Empty -> fprintf ppf ""
    | Element (x, set') -> fprintf ppf "%a, %a" Path.fprint x fprint set'

end



(* -------------------------------------------------------------------------
   UNIFICATION OVER KINDS

   The module [Ukind] implements a representation of kinds with variables
   and unification.
 *)

module Ukind = struct

  type desc =
      Var
    | Ulevel
    | Utype
    | Urow of Uset.t

  and node = { mutable desc: desc }

  and t = node Proxy.t



  (* [create] generates a new kind with the given descriptor. *)

  let create desc =
    Proxy.create { desc = desc }

  let level = create Ulevel
  let typ = create Utype
  let row = create (Urow (Uset.create Uset.Empty))



  (* [unify] unifies two kinds. 
     It raises [Unify] or [Uset.Unify] or [Uset.Forbidden] if the
     two kinds are not unifiable. 
   *)

  exception Unify

  let unify kind1 kind2 =

    let kind1n = Proxy.desc kind1
    and kind2n = Proxy.desc kind2 in

    begin match kind1n.desc, kind2n.desc with
      _, Var -> ()
    | Var, _ -> kind1n.desc <- kind2n.desc
    | Ulevel, Ulevel -> ()
    | Utype, Utype -> ()
    | Urow set1, Urow set2 -> Uset.unify set1 set2
    | _ -> raise Unify
    end;

    Proxy.linksto kind2 kind1



  (* [freeze] converts a kind into an element of type [t].  It raises
     [Freeze] if the given kind is a variable or [Uset.Freeze] if it
     contains a set variable.
   *)

  exception Freeze

  let freeze kind =
    match (Proxy.desc kind).desc with
      Var -> raise Freeze
    | Ulevel -> Level
    | Utype -> Type
    | Urow set -> Row (Uset.freeze set)



  (* [unfreeze] convert an element of type [t] into its representation
     in this module.
   *)

  let unfreeze = function
      Level -> create Ulevel
    | Type -> create Utype
    | Row list -> create (Urow (Uset.unfreeze list))



  (* Pretty-printing : [fprint] *)

  let fprint ppf kind =
    match (Proxy.desc kind).desc with
      Var -> fprintf ppf "?"
    | Ulevel -> fprintf ppf "level"
    | Utype -> fprintf ppf "type"
    | Urow set -> fprintf ppf "row[%a]" Uset.fprint set

end
