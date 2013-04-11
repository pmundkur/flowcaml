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

(* $Id: misc.ml,v 1.2 2003/06/26 13:33:00 simonet Exp $ *)
(* Misc: *)

let name_of_int i =
  if i < 26
  then String.make 1 (Char.chr (i+97))
  else String.make 1 (Char.chr ((i mod 26) + 97)) ^ string_of_int (i/26)

let iter2i f l1 l2 =
  let rec iter2i_rec i l1 l2 =
    match l1, l2 with
      [], [] -> ()
    | hd1 :: tl1, hd2 :: tl2 ->
	f i hd1 hd2;
	iter2i_rec (i + 1) tl1 tl2
    | [], _ :: _ | _ :: _, [] -> invalid_arg "Misc.iter2i"
  in
  iter2i_rec 0 l1 l2

let rec map_end f l1 l2 =
  match l1 with
    [] -> l2
  | hd::tl -> f hd :: map_end f tl l2


let rec map3 f l1 l2 l3 =
  match l1, l2, l3 with
    [], [], [] -> []
  | hd1 :: tl1, hd2 :: tl2, hd3 :: tl3 ->
      (f hd1 hd2 hd3) :: (map3 f tl1 tl2 tl3)
  | _ -> invalid_arg "Misc.map3"

let fprint_list sep f ppf list =
  let rec aux ppf = function
      [] -> ()
    | hd :: [] -> f ppf hd
    | hd :: tl ->
	Format.fprintf ppf "%a%t%a" f hd sep aux tl
  in
  aux ppf list

let command cmdline =
  if !Clflags.verbose then begin
    prerr_string "+ ";
    prerr_string cmdline;
    prerr_newline()
  end;
  Sys.command cmdline

(* -------------------------------------------------------------------------
   Errors 
 *)

exception Fatal_error

let fatal_error msg =
  prerr_string ">> Fatal error: "; prerr_endline msg; 
  exit 2
  (* raise Fatal_error *)


type error =
    Not_implemented of string

exception Error of error

let not_implemented msg =
  raise (Error (Not_implemented msg))

let report_error ppf = function
    Not_implemented msg ->
      Format.fprintf ppf "Feature not yet available: %s" msg



(* -------------------------------------------------------------------------
   Hashtable functions
 *)

let create_hashtable size init =
  let tbl = Hashtbl.create size in
  List.iter (fun (key, data) -> Hashtbl.add tbl key data) init;
  tbl



(* -------------------------------------------------------------------------
   Testing unicity
 *)

let unicity compare list error =

  let rec loop = function
      [] | [_] -> ()
    | x :: ((y :: _) as tl) ->
	if compare x y = 0 then error x y
	else loop tl
  in

  loop (List.sort compare list)



(* -------------------------------------------------------------------------
   Filesystem
 *)

let remove_file filename =
  try
    Sys.remove filename
  with Sys_error msg ->
    ()

(* Expand a -I option: if it starts with +, make it relative to the standard
   library directory *)

let expand_directory alt s =
  if String.length s > 0 && s.[0] = '+'
  then Filename.concat alt
                       (String.sub s 1 (String.length s - 1))
  else s


let find_in_path path name =
  if not (Filename.is_implicit name) then
    if Sys.file_exists name then name else raise Not_found
  else begin
    let rec try_dir = function
      [] -> raise Not_found
    | dir::rem ->
        let fullname = Filename.concat dir name in
        if Sys.file_exists fullname then fullname else try_dir rem
    in try_dir path
  end
