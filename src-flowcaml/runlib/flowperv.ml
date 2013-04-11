(**************************************************************************)
(*                                                                        *)
(*                               Flow Caml                                *)
(*                                                                        *)
(*          Vincent Simonet, Projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*  Copyright 2002, 2003 Institut National de Recherche en Informatique   *)
(*  et en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with the   *)
(*  special exception on linking described in file ../LICENSE.            *)
(*                                                                        *)
(*  Author contact: Vincent.Simonet@inria.fr                              *)
(*  Software page: http://cristal.inria.fr/~simonet/soft/flowcaml/        *)
(*                                                                        *)
(**************************************************************************)

(* $Id: flowperv.ml,v 1.6 2003/06/27 13:09:57 simonet Exp $ *)



(***************************************************************************)
(** {2 Strings} *)

type charray = string

let (^^) = (^)
let ($$) s i = s.[i]
let charray_of_string = String.copy
let string_of_charray = String.copy



(***************************************************************************)
(** {2 Exceptions} *)

(** [_propagate_ (fun () -> e1) exn] is an implementation of the 
    construct [try ... with exn -> e1 propagate]. *)
let _propagate_ f exn =
  begin try ignore (f ()) with _ -> () end;
  raise exn



(** [try_finally (fun () -> e1) (fun () -> e2)] is an implementation of the
    construct [try e1 finally e2]. *)
let _try_finally_ f1 f2 =
  let x1 =
    try 
      f1 ()
    with
      exn -> 
	begin try ignore (f2 ()) with _ -> () end;
	raise exn
  in
  f2 ();
  x1



(** [catchable exn] tests wether the exception [exn] may be catched. *)
let _catchable_ = function
    Out_of_memory
  | Stack_overflow
  | Assert_failure _
  | Match_failure _ -> false
  | Invalid_argument tag ->
      let len = String.length tag in
      not ((len > 6 && String.sub tag 0 6 = "Array.")
             or (len > 7 && String.sub tag 0 7 = "String."))
  | _ -> true
