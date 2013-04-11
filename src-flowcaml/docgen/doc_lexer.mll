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

(* $Id: doc_lexer.mll,v 1.6 2003/06/26 13:32:48 simonet Exp $ *)


{

open Doc_parser

exception Error of string

let in_special_comment = ref false

let open_blank blank =
  let rec count accu i =
    if i < 0 then 0
    else match blank.[i] with
      '\010' ->
	if accu then count false (i - 1)
	else i + 1
    | _ ->
	count accu (i - 1)
  in
  count true (String.length blank - 1)


let close_blank blank =
  let len = String.length blank - 1 in
  let rec count accu i =
    if i > len then len
    else match blank.[i] with
      '\010' ->
	if accu then count false (i + 1)
	else i - 1
    | _ ->
	count accu (i + 1)
  in
  count true 0


let verb_buffer = Buffer.create 7
let verb_depth = ref 0
    
} 

let blank = [' ' '\010' '\013' '\009' '\012']
let alphanumeric = ['a'-'z' 'A'-'Z' '0'-'9']


rule code = parse
  blank* "(***"
    { comment lexbuf; code lexbuf }

| blank* "(**/**)" blank*
    { TERMINATE (Lexing.lexeme_start lexbuf, Lexing.lexeme_end lexbuf) }

| blank* "(**"
    { in_special_comment := true;
      OPEN (Lexing.lexeme_start lexbuf + open_blank (Lexing.lexeme lexbuf))
    }

| "(*"
    { comment lexbuf; code lexbuf }

| '"'
    { string lexbuf; code lexbuf }

| eof
    { EOF }

| _
    { code lexbuf }



and comment = parse

  "*)"
    { () }

| "(*"
    { comment lexbuf; comment lexbuf }

| '"'
    { string lexbuf; comment lexbuf }

| eof
    { raise (Error "Unterminated comment") }

| _
    { comment lexbuf }



and string = parse

  "\\\""
    { string lexbuf }

| '"'
    { () }

| eof
    { raise (Error "Unterminated string") }

| _
    { string lexbuf }



and special_comment = parse
  "*)" blank*
    { in_special_comment := false;
      CLOSE (Lexing.lexeme_start lexbuf + close_blank (Lexing.lexeme lexbuf) + 1)
    }

| "(*"
    { comment lexbuf; special_comment lexbuf }

| "\""
    { string lexbuf; special_comment lexbuf }

| "{" (alphanumeric+ | ['!' '^' '_']) ' '*
    { let s = Lexing.lexeme lexbuf in
      let s' =
	try String.sub s 0 (String.index s ' ')
	with Not_found -> s
      in
      LBRACE (String.sub s 1 (String.length s' - 1))
    }

| "}"
    { RBRACE }

| "@" alphanumeric*
    { let s = Lexing.lexeme lexbuf in
      DIRECTIVE (String.sub s 1 (String.length s - 1))
    }
| "[" 
    { 
      Buffer.clear verb_buffer;
      verb lexbuf;
      VERB (Buffer.contents verb_buffer)
    }

| ['*' '(' ')']
    { TEXT (Lexing.lexeme lexbuf) }

| '\\' _
    { TEXT (String.sub (Lexing.lexeme lexbuf) 1 1) }

| ([^ '{' '}' '(' ')' '*' '[' '"' '@' '\\'])+
    { TEXT (Lexing.lexeme lexbuf) }

| eof
    { raise (Error "Unterminated special comment") }



and verb = parse
  "["
    { Buffer.add_char verb_buffer '[';
      incr verb_depth;
      verb lexbuf
    }

| "]"
    { if !verb_depth = 0 then () else begin
        decr verb_depth;
        Buffer.add_char verb_buffer ']';
	verb lexbuf
      end
    }

| eof
    { raise (Error "Unterminated verbatim") }

| [^ '[' ']']
    { Buffer.add_string verb_buffer (Lexing.lexeme lexbuf);
      verb lexbuf
    } 

    
  
{
 let init () =
   in_special_comment := false

 let token lexbuf =
   if !in_special_comment then special_comment lexbuf
   else code lexbuf
}

