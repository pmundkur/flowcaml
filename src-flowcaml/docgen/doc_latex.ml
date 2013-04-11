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

(* $Id: doc_latex.ml,v 1.10 2003/06/26 13:32:48 simonet Exp $ *)

open Format
open Parsetree
open Types
open Print_types
open Doc_parsetree



let warning msg =
  eprintf "Warning: %s\n" msg;
  flush stderr
    


(***************************************************************************)
(** {2 Interface with Format} *)

let equip_formatter ppf =
  pp_set_formatter_tag_functions ppf
    { Format.mark_open_tag = (fun _ -> "");
      Format.mark_close_tag = (fun _ -> "");
      Format.print_open_tag = (fun _ -> ());
      Format.print_close_tag = (fun _ -> ())
    }



(***************************************************************************)
(** {2 Formatted text} *)

let rec ftext ppf c =
  List.iter (ftext_item ppf) c
	
and ftext_item ppf = function
    Directive s ->
      begin match s with
	"deprecated" -> fprintf ppf "Deprecated. "
      | _ -> warning (sprintf "Unknown directive: \"%s\"" s)
      end
  | String s -> fprintf ppf "%s" s
  | Block (tag, c) -> 
      let f s1 s2 =
	fprintf ppf "%s%a%s" s1 ftext c s2
      in
      match tag with
	"b" -> f "\\textbf{" "}"
      | "i" -> f "\\textit{" "}"
      | "src" | "!" -> f "\\verb`" "`"
      | "2" -> f "\\subsection{" "}"
      | "3" -> f "\\subsubsection{" "}"
      | "6" -> f "\\paragraph{" "}"
      | "_" -> f "$_{\\rm " "}$"
      | "^" -> f "$^{\\rm " "}$"
      | _ -> 
	  warning (sprintf "Unknown tag: \"%s\"" tag);
	  f "" ""




(***************************************************************************)
(** {2 Document markup} *)

let begin_interface ppf modulename =
  fprintf ppf "\\section{Module \\texttt{%s}}@ " modulename

let begin_signature ppf =
  fprintf ppf "\\begin{fdoc-signature}@ "

let end_signature ppf =
  fprintf ppf "\\end{fdoc-signature}@ "

let begin_signature_item ppf =
  fprintf ppf "\\fdocsignatureitem@ "

let end_signature_item ppf =
  ()



(***************************************************************************)
(** {2 Signature items} *)

let comment ppf ft =
  fprintf ppf "\\fdocsignatureitem@ ";
  fprintf ppf "\\begin{fdoc-comment}@ \
               @[<hov>%a@]@ \
	       \\end{fdoc-comment}@ @ " ftext ft



let description_after ppf = function
    [] -> ()
  | ft ->
      fprintf ppf "\\begin{fdoc-description-after}@ ";
      fprintf ppf "@[<hov>%a@]@ " ftext ft;
      fprintf ppf "\\end{fdoc-description-after}@ "

let description_before  ppf = function
    [] -> ()
  | ft ->
      fprintf ppf "\\begin{fdoc-description-before}@ ";
      fprintf ppf "@[<hov>%a@]@ " ftext ft;
      fprintf ppf "\\end{fdoc-description-before}@ "
	
let item style fprinter ppf name desc ft =
  begin_signature_item ppf;
  fprintf ppf "\\begin{lstlisting}[style=%s]{}@ " "fdoc";
  fprintf ppf "@[<2>%a@]@ " (fprinter (Ident.create name)) desc;
  fprintf ppf "\\end{lstlisting}@ ";
  description_after ppf ft;
  fprintf ppf "@ "

let value_description ppf name vald ft = 
  begin_signature_item ppf;
  fprintf ppf "\\begin{lstlisting}[style=%s]{}@ " "fdoc";
  fprintf ppf "@[<2>%a@]@ " (fprint_value_description (Ident.create name)) vald;
  fprintf ppf "\\end{lstlisting}@ ";
  if !Doc_images.eps_output or !Doc_images.eps_output then begin
    let basename = Doc_images.value_description vald in
    fprintf ppf "\\fdocgraphicscheme{%s}@ " basename;
  end;
  description_after ppf ft;
  fprintf ppf "@ "

let type_declaration = item "type" fprint_type_declaration
let level_declaration = item "level" fprint_level_declaration
let exception_declaration = item "exn" fprint_exception_declaration


let begin_module ppf name =
  fprintf ppf "\\begin{lstlisting}[style=fdoc]{}@ ";
  fprintf ppf "module %s : @[<hv>" name

let end_module ppf tf =
  fprintf ppf "\\end{lstlisting}@ %a@ " description_after tf



let modtype_abstract ppf name ft =
  item "modtype" (fun id ppf () -> Ident.fprint ppf id) ppf name () ft

let begin_modtype ppf name =
  fprintf ppf "\\begin{lstlisting}[style=fdoc]{}@ ";
  fprintf ppf "module type %s = @[<hv>" name

let end_modtype ppf tf =
  fprintf ppf "\\end{lstlisting}@ %a@ " description_after tf



let begin_long ppf typename name ft =
  fprintf ppf "\\begin{fdoc-longmodule}{%s}{%s}@ " typename name;
  description_before ppf ft

let end_long ppf =
  fprintf ppf "\\end{fdoc-longmodule}@ @ "



let module_ident ppf path =
  fprintf ppf "%a@]@ " Path.fprint path

let module_sig_begin ppf =
  fprintf ppf "sig@]@ ";
  end_module ppf []

let module_sig_end ppf =
  fprintf ppf "\\begin{lstlisting}[style=fdoc]{}@ ";
  fprintf ppf "end@ "

let functor_arrow ppf arg_id arg_mty pci pcf =
  let fprint_arrow ppf =
    match Level.Set.is_empty pci, Level.Set.is_empty pcf with
      true, true -> 
	Format.fprintf ppf "->"
    | false, true ->
	Format.fprintf ppf "-{from @[%a@]}->" Level.Set.fprint pci
    | true, false ->
	Format.fprintf ppf "-{to @[%a@]}->" Level.Set.fprint pcf
    | false, false ->
	Format.fprintf ppf "-{from @[%a@] to @[%a@]}->"
	  Level.Set.fprint pci
	  Level.Set.fprint pcf
  in
  fprintf ppf "@[<2>functor@ (%a : %a) %t@]@ "
    Ident.fprint arg_id
    fprint_module_type arg_mty
    fprint_arrow

let constraints ppf list =
  ignore (List.fold_left (fun _and pwth ->
    if _and then fprintf ppf "@  and ";
    begin match pwth.pwth_desc with
      Pwith_type decl ->
	let params ppf =
	  match decl.ptype_params with
	    [] -> ()
	  | [name, _] -> fprintf ppf "'%s " name
	  | (name, _) :: tl -> 
	      fprintf ppf "('%s" name;
	      List.iter (function name, _ -> fprintf ppf ", %s" name) tl;
	      fprintf ppf ") ";
	in
	let rec fprint_ptyp ppf ptyp =
	  match ptyp.ptyp_desc with
	    Ptyp_var name -> fprintf ppf "'%s" name
	  | Ptyp_arrow _
	  | Ptyp_tuple _ 
	  | Ptyp_bounds _ 
	  | Ptyp_row _ 
	  | Ptyp_arrow_abbrev _ -> assert false
	  | Ptyp_constr (lid, ptyp_list) ->
	      begin match ptyp_list with
		[] -> ()
	      | [ptyp'] -> fprintf ppf "%a " fprint_ptyp ptyp'
	      | hd :: tl ->
		  fprintf ppf "(%a " fprint_ptyp hd;
		  List.iter (function ptyp' -> 
		    fprintf ppf "%a, " fprint_ptyp ptyp'
                  ) tl;
		  fprintf ppf ") "
	      end;
	      Longident.fprint ppf lid
	  | Ptyp_paren ptyp' -> 
	      fprintf ppf "(%a)" fprint_ptyp ptyp'
	in
	fprintf ppf "type %t%a = %a" 
	  params
	  Longident.fprint pwth.pwth_ident
	  fprint_ptyp (match decl.ptype_manifest with
	    None -> assert false | Some ptyp -> ptyp)
    | Pwith_level ld ->
	assert false (* TEMPORARY *)
    | Pwith_module lid' -> 
	fprintf ppf "module %a = %a " 
	  Longident.fprint pwth.pwth_ident 
	  Longident.fprint lid'
    end;
    true
  ) false list)

let with_constraint ppf lid list =
  fprintf ppf "%a @[<hv>with %a@]@]@ " 
    Longident.fprint lid 
    constraints list
