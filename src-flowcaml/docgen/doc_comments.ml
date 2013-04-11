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

(* $Id $ *)

open Location
open Doc_parsetree



let comments : comment list ref = ref []

let extract filename =
  let ic = open_in filename in
  Doc_lexer.init ();
  let lexbuf = Lexing.from_channel ic in
  try
    comments := Doc_parser.file Doc_lexer.token lexbuf;
    close_in ic
  with
    Doc_lexer.Error msg ->
      Printf.eprintf "File %s, lexing error:\n%s\n"
	filename
	msg;
      exit 2
  | Parsing.Parse_error ->
      Printf.eprintf "File %s, characters %i-%i:\nParse error\n"
	filename
	(Lexing.lexeme_start lexbuf)
	(Lexing.lexeme_end lexbuf);
      exit 2



exception TerminateComment

let rec find skip loc = 
  match !comments with
    [] -> []
  | ({ cmt_content = Terminate } as hd) :: tl ->
      if hd.cmt_start < loc.loc_start then 
	(comments := tl; raise TerminateComment)
      else []
  | ({ cmt_content = Ftext ft } as hd) :: tl ->  
      if hd.cmt_end < loc.loc_start 
      then (skip ft; comments := tl; find skip loc)
      else if hd.cmt_start <= loc.loc_end 
      then (comments := tl; ft)
      else []


let find_forward skip loc =
  let rec look_forward accu = function
      [] -> raise Not_found
    | hd :: tl ->
	if hd.cmt_start > loc.loc_end then raise Not_found
	else
	  if hd.cmt_start = loc.loc_end then begin
	    match hd.cmt_content with
	      Terminate -> raise Not_found
	    | Ftext ft ->
		comments := List.rev_append accu tl;
		ft
	  end
	  else look_forward (hd :: accu) tl
  in
  try
    look_forward [] !comments
  with
    Not_found -> find skip loc
