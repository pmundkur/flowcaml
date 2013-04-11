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

(* $Id: level.ml,v 1.3 2003/06/26 13:32:56 simonet Exp $ *)
(* Level: dealing with security levels *)

open Format



type t = 
    Tlvl_principal of string
  | Tlvl_path of Path.t

type level = t


let compare level1 level2 =
  match level1, level2 with
    Tlvl_principal name1, Tlvl_principal name2 -> Pervasives.compare name1 name2
  | Tlvl_path path1, Tlvl_path path2 -> Path.compare path1 path2
  | Tlvl_principal _, Tlvl_path _ -> -1
  | Tlvl_path _, Tlvl_principal _ -> 1

let hash = function
    Tlvl_principal name -> Hashtbl.hash name
  | Tlvl_path path -> Path.hash path

let fprint ppf = function
    Tlvl_principal name ->
      fprintf ppf "!%s" name
  | Tlvl_path path ->
      Path.fprint ppf path

let to_string level =
  fprint Format.str_formatter level;
  Format.flush_str_formatter ()



module Set = struct

  include Set.Make (struct
    type t = level
    let compare = compare
  end)

  let map f set =
    fold (fun level set' -> add (f level) set') set empty

  let fprint ppf set =

    ignore begin fold (fun level first ->
      if not first then fprintf ppf ",@ ";
      fprint ppf level;
      false
    ) set true;
    end

end
