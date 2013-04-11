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

(* $Id: parse.ml,v 1.2 2003/06/26 13:32:51 simonet Exp $ *)
(* Parse: Entry points in the parser *)

open Location



(* Skip tokens to the end of the phrase *)

let rec skip_phrase lexbuf =
  try
    match Lexer.token lexbuf with
      Parser.SEMISEMI | Parser.EOF -> ()
    | _ -> skip_phrase lexbuf
  with
    | Lexer.Error (Lexer.Unterminated_comment, _, _) -> ()
    | Lexer.Error (Lexer.Unterminated_string, _, _) -> ()
    | Lexer.Error (Lexer.Unterminated_string_in_comment, _, _) -> ()
    | Lexer.Error (Lexer.Illegal_character _,_,_) -> skip_phrase lexbuf
;;

let maybe_skip_phrase lexbuf =
  if Parsing.is_current_lookahead Parser.SEMISEMI
  || Parsing.is_current_lookahead Parser.EOF
  then ()
  else skip_phrase lexbuf

let wrap parsing_fun lexbuf =
  try
    let ast = parsing_fun Lexer.token lexbuf in
    Parsing.clear_parser();
    ast
  with
  | Lexer.Error(Lexer.Unterminated_comment, _, _) as err -> raise err
  | Lexer.Error(Lexer.Unterminated_string, _, _) as err -> raise err
  | Lexer.Error(Lexer.Unterminated_string_in_comment, _, _) as err -> raise err
  | Lexer.Error(Lexer.Illegal_character _, _, _) as err ->
      if !Location.input_name = "" then skip_phrase lexbuf;
      raise err
  | Syntaxerr.Error _ as err ->
      if !Location.input_name = "" then maybe_skip_phrase lexbuf;
      raise err
  | Parsing.Parse_error | Syntaxerr.Escape_error ->
      let loc = { loc_start = Lexing.lexeme_start lexbuf;
                  loc_end = Lexing.lexeme_end lexbuf;
                  loc_ghost = false } in
      if !Location.input_name = "" 
      then maybe_skip_phrase lexbuf;
      raise(Syntaxerr.Error(Syntaxerr.Other loc))
;;

let implementation = wrap Parser.implementation
and interface = wrap Parser.interface
and toplevel_phrase = wrap Parser.toplevel_phrase
and use_file = wrap Parser.use_file
