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

(* $Id: ident.ml,v 1.4 2003/06/26 13:32:56 simonet Exp $ *)
(* Ident *)




open Format

type t = { stamp: int; name: string; mutable global: bool }

(* A stamp of 0 denotes a persistent identifier *)

let currentstamp = ref 0

let create s =
  incr currentstamp;
  { name = s; stamp = !currentstamp; global = false }

let create_persistent s =
  { name = s; stamp = 0; global = true }

let rename i =
  incr currentstamp;
  { i with stamp = !currentstamp }

let name i = i.name

let unique_name i = i.name ^ "_" ^ string_of_int i.stamp

let persistent i = (i.stamp = 0)

let equal i1 i2 = i1.name = i2.name

let same i1 i2 = i1 = i2
  (* Possibly more efficient version (with a real compiler, at least):
       if i1.stamp <> 0
       then i1.stamp = i2.stamp
       else i2.stamp = 0 && i1.name = i2.name *)

let binding_time i = i.stamp

let current_time() = !currentstamp
let set_current_time t = currentstamp := max !currentstamp t

let hide i =
  { i with stamp = -1 }

let make_global i =
  i.global <- true

let global i =
  i.global

let print ppf i =
  match i.stamp with
  | 0 -> fprintf ppf "%s!" i.name
  | -1 -> fprintf ppf "%s#" i.name
  | n -> fprintf ppf "%s/%i%s" i.name n (if i.global then "g" else "")

type 'a tbl =
    Empty
  | Node of 'a tbl * 'a data * 'a tbl * int

and 'a data =
  { ident: t;
    data: 'a;
    previous: 'a data option }

let empty = Empty

(* Inline expansion of height for better speed
 * let height = function
 *     Empty -> 0
 *   | Node(_,_,_,h) -> h
 *)

let mknode l d r =
  let hl = match l with Empty -> 0 | Node(_,_,_,h) -> h
  and hr = match r with Empty -> 0 | Node(_,_,_,h) -> h in
  Node(l, d, r, (if hl >= hr then hl + 1 else hr + 1))

let balance l d r =
  let hl = match l with Empty -> 0 | Node(_,_,_,h) -> h
  and hr = match r with Empty -> 0 | Node(_,_,_,h) -> h in
  if hl > hr + 1 then
    match l with
    | Node (ll, ld, lr, _)
      when (match ll with Empty -> 0 | Node(_,_,_,h) -> h) >=
           (match lr with Empty -> 0 | Node(_,_,_,h) -> h) ->
        mknode ll ld (mknode lr d r)
    | Node (ll, ld, Node(lrl, lrd, lrr, _), _) ->
        mknode (mknode ll ld lrl) lrd (mknode lrr d r)
    | _ -> assert false
  else if hr > hl + 1 then
    match r with
    | Node (rl, rd, rr, _)
      when (match rr with Empty -> 0 | Node(_,_,_,h) -> h) >=
           (match rl with Empty -> 0 | Node(_,_,_,h) -> h) ->
        mknode (mknode l d rl) rd rr
    | Node (Node (rll, rld, rlr, _), rd, rr, _) ->
        mknode (mknode l d rll) rld (mknode rlr rd rr)
    | _ -> assert false
  else
    mknode l d r

let rec merge t1 t2 =
  match (t1, t2) with
    (Empty, t) -> t
  | (t, Empty) -> t
  | (Node(l1, k1, r1, h1), Node(l2, k2, r2, h2)) ->
      balance l1 k1 (balance (merge r1 l2) k2 r2)


let rec add id data = function
    Empty ->
      Node(Empty, {ident = id; data = data; previous = None}, Empty, 1)
  | Node(l, k, r, h) ->
      let c = compare id.name k.ident.name in
      if c = 0 then
        Node(l, {ident = id; data = data; previous = Some k}, r, h)
      else if c < 0 then
        balance (add id data l) k r
      else
        balance l k (add id data r)

let rec find_stamp s = function
    None ->
      raise Not_found
  | Some k ->
      if k.ident.stamp = s then k.data else find_stamp s k.previous

let rec find_same id = function
    Empty ->
      raise Not_found
  | Node(l, k, r, _) ->
      let c = compare id.name k.ident.name in
      if c = 0 then
        if id.stamp = k.ident.stamp
        then k.data
        else find_stamp id.stamp k.previous
      else
        find_same id (if c < 0 then l else r)

let rec find_name name = function
    Empty ->
      raise Not_found
  | Node(l, k, r, _) ->
      let c = compare name k.ident.name in
      if c = 0 then
        k.data
      else
        find_name name (if c < 0 then l else r)

let rec remove_stamp s k =
  if k.ident.stamp = s then k.previous
  else match k.previous with
    None -> Some k
  | Some k' -> Some { k with previous = remove_stamp s k' }

let rec remove id = function
    Empty -> 
      Empty
  | Node (l, k, r, h) -> 
      let c = compare id.name k.ident.name in
      if c = 0 then begin
	match remove_stamp id.stamp k with
	  None -> merge l r
	| Some k' -> Node (l, k', r, h)
      end
      else if c < 0 then
	balance (remove id l) k r
      else
	balance l k (remove id r)

let rec map_hz f k =
  { k with
    data = f k.data;
    previous = Option.map (map_hz f) k.previous
  } 

let rec map f = function
    Empty ->
      Empty
  | Node(l, k, r, h) -> 
      Node(map f l, map_hz f k, map f r, h)

let rec iter_hz f k =
  f k.data;
  Option.iter (iter_hz f) k.previous

let rec iter f = function
    Empty ->
      ()
  | Node(l, k, r, h) -> 
      iter f l;
      iter_hz f k;
      iter f r

let rec fold_hz f k accu =
  match k.previous with
    None -> f k.ident k.data accu
  | Some k' -> f k.ident k.data (fold_hz f k' accu)

let rec fold f m accu =
  match m with
    Empty -> accu
  | Node(l, k, r, _) ->
      fold f l (fold_hz f k (fold f r accu))

let fprint ppf i = 
  if ! Clflags.debug then print ppf i
  else begin
    if List.mem i.name
	["or"; "mod"; "land"; "lor"; "lxor"; "lsl"; "lsr"; "asr"] then
      fprintf ppf "( %s )" i.name
    else
      match i.name.[0] with
	'a'..'z' | 'A'..'Z' | '\223'..'\246' | '\248'..'\255' | '_' ->
          fprintf ppf "%s" i.name
      | _ -> fprintf ppf "( %s )" i.name
  end

let stamp i =
  i.stamp

let create_unsafe s i =
  { name = s; stamp = i; global = false }
