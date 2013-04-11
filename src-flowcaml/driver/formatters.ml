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

(* $Id: formatters.ml,v 1.7 2003/06/26 13:32:49 simonet Exp $ *)
(* Formatters. *)

open Terminfo
open Format

(***************************************************************************)

let colorized = ref true

let parse_pprint s =
  for i = 0 to String.length s - 1 do
    match s.[i] with
    | 'c' -> colorized := false
    | 'C' -> colorized := true
    | '0' ->
	Solver.Printing.all_variables := false;
	Solver.Printing.ghost_variables := true
    | '1' ->
	Solver.Printing.all_variables := false;
	Solver.Printing.ghost_variables := false	
    | '2' ->
	Solver.Printing.all_variables := true;
	Solver.Printing.ghost_variables := false
    | 'v' -> Solver.Printing.polarities := false
    | 'V' -> Solver.Printing.polarities := true
    | _ -> ()
  done

(* default *)
let _ =
  parse_pprint "Cv1"



(***************************************************************************)

type terminfo =
    { covariant_open: string;
      covariant_close: string;
      contravariant_open: string;
      contravariant_close: string;
      invariant_open: string;
      invariant_close: string;
      highlight_open: string;
      highlight_close: string;
      input_open: string;
      input_close: string;
    }

let open_sequence ti tag =
  if not !colorized then "" else
  match tag with
    "+" -> ti.covariant_open
  | "-" -> ti.contravariant_open
  | "=" -> ti.invariant_open
  | "hi" -> ti.highlight_open
  | _ -> ""

let close_sequence ti tag =
  if not !colorized then "" else 
  match tag with
    "+" -> ti.covariant_close
  | "-" -> ti.contravariant_close
  | "=" -> ti.invariant_close
  | "hi" -> ti.highlight_close
  | _ -> ""



let terminfo_vt100 =
  { covariant_open = "\027[31m";
    covariant_close = "\027[39m";
    contravariant_open = "\027[32m";
    contravariant_close = "\027[39m";
    invariant_open = ""; (* "\027[33m" *)
    invariant_close = ""; (* "\027[39m" *)
    highlight_open = "\027[1m";
    highlight_close = "\027[0m";
    input_open = "\027[34m";
    input_close = "\027[39m";
  }

let terminfo_default =
  { covariant_open = "";
    covariant_close = "";
    contravariant_open = "";
    contravariant_close = "";
    invariant_open = "";
    invariant_close = "";
    highlight_open = "";
    highlight_close = "";
    input_open = "";
    input_close = "";
  }


let terminfo = ref terminfo_default

let set_term termname =
  match termname with
    "vt100" | "xterm" | "xterm-color" -> terminfo := terminfo_vt100
  | _ -> terminfo := terminfo_default



let equip ppf =

  let stack = Stack.create () in
  Stack.push "#dumb#" stack;

  let open_tag tag =
    let close = close_sequence !terminfo (Stack.top stack) in
    Stack.push tag stack;
    close ^ open_sequence !terminfo tag
  in

  let close_tag tag =
    ignore (Stack.pop stack);
    let reopen = open_sequence !terminfo (Stack.top stack) in
    close_sequence !terminfo tag ^ reopen
  in

  pp_set_formatter_tag_functions ppf 
    { mark_open_tag = open_tag;
      mark_close_tag = close_tag;
      print_open_tag = (function _ -> ());
      print_close_tag = (function _ -> ())
    } 


let _ =
  equip std_formatter;
  equip err_formatter;
  set_term Config.default_term
