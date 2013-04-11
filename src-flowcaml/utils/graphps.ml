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

open Printf;;

type command =
   | Open_graph of int * int
   | Close_graph

   | Set_rgb_color of int * int * int
   | Plot of int * int
   | Moveto of int * int
   | Lineto of int * int
   | Rmoveto of int * int
   | Rlineto of int * int

   | Draw_rect of int * int * int * int
   | Fill_rect of int * int * int * int

   | Draw_poly of (int * int) array
   | Fill_poly of (int * int) array

   | Draw_arc of int * int * int * int * int * int
   | Fill_arc of int * int * int * int * int * int

   | Draw_ellipse of int * int * int * int
   | Fill_ellipse of int * int * int * int

   | Draw_circle of int * int * int
   | Fill_circle of int * int * int

   | Set_line_width of int

   | Draw_char of char
   | Draw_string of string
   | Set_font of string * int
;;

let eps_mode = ref false;;
let ps_out_channel = ref stdout;;

let open_ps s = ps_out_channel := open_out s; eps_mode := false;;
let open_eps s = open_ps s; eps_mode := true;;

let current_ps_program = ref [];;

let add_command c = current_ps_program := c :: !current_ps_program;;

let size_x_ref = ref 640 and size_y_ref = ref 640;;

let parse_geometry s =
  let lim = String.length s in
  let rec find_x i =
   if i >= lim then raise Not_found else
   if s.[i] = 'x' then i else find_x (i + 1) in
  let rec find_digit i =
   if i >= lim then raise Not_found else
   match s.[i] with
   | '0' .. '9' -> i
   | _ -> find_digit (i + 1) in
  try
   let ix = find_x 0 in
   let dx = find_digit 0 in
   let sx = String.sub s dx (ix - dx) in
   let dy = find_digit ix in
   let sy = String.sub s dy (lim - dy) in
   int_of_string sx, int_of_string sy
  with Not_found -> (640, 640)
     | Failure _ -> (640, 640);;

let open_graph s =
 let x, y = parse_geometry s in
 size_x_ref := x; size_y_ref := y;
 add_command (Open_graph (x, y));;

let clear_graph () = current_ps_program := [];;
let size_x () = !size_x_ref;;
let size_y () = !size_y_ref;;

(* Colors *)

type color = int;;

let rgb r g b = (r lsl 16) + (g lsl 8) + b;;

let rgb_of_color c =
 let r = (c lsr 16) land 0xFF in
 let g = (c lsr 8) land 0xFF in
 let b = c land 0xFF in
 r, g, b;;

let black   = 0x000000
and white   = 0xFFFFFF
and red     = 0xFF0000
and green   = 0x00FF00
and blue    = 0x0000FF
and yellow  = 0xFFFF00
and cyan    = 0x00FFFF
and magenta = 0xFF00FF;;

let background = white
and foreground = black;;

let set_color c =
 let c = if c < 0 then background else c in
 let r, g, b = rgb_of_color c in
 add_command (Set_rgb_color (r, g, b));;

(* Drawing *)

let plot x y = add_command (Plot (x, y));;

let curr_x = ref 0;;
let curr_y = ref 0;;

let current_x () = !curr_x;;
let current_y () = !curr_y;;

let current_point () = !curr_x, !curr_y;;

let set_point x y = curr_x := x; curr_y := y;;

let moveto x y =
 set_point x y;
 add_command (Moveto (x, y));;

let lineto x y =
 set_point x y;
 add_command (Lineto (x, y));;

let rlineto dx dy = 
 set_point (current_x () + dx) (current_y () + dy);
 add_command (Rlineto (dx, dy));;

let rmoveto dx dy =
 set_point (current_x () + dx) (current_y () + dy);
 add_command (Rmoveto (dx, dy));;

let draw_arc x y rx ry a1 a2 = add_command (Draw_arc (x, y, rx, ry, a1, a2));;
let draw_ellipse x y rx ry = add_command (Draw_ellipse (x, y, rx, ry));;
let draw_circle x y r = add_command (Draw_circle (x, y, r));;

let set_line_width w = add_command (Set_line_width w);;

let draw_rect x y w h = add_command (Draw_rect (x, y, w, h));;
let fill_rect x y w h = add_command (Fill_rect (x, y, w, h));;
let fill_poly v = add_command (Fill_poly v);;
let draw_poly v = add_command (Draw_poly v);;

let fill_arc x y rx ry a1 a2 = add_command (Fill_arc (x, y, rx, ry, a1, a2));;

let fill_ellipse x y rx ry = add_command (Fill_ellipse (x, y, rx, ry));;
let fill_circle x y r = add_command (Fill_circle (x, y, r));;

(* Text *)
let default_font = "Helvetica-Bold";;
let default_font_size = 10;;

let x_text_size = ref default_font_size;;
let y_text_size = ref default_font_size;;

let draw_char c =
 let x = current_x ()
 and y = current_y () in
 add_command (Draw_char c);
 moveto (x + !x_text_size) y;;
(* set_point (x + !x_text_size) y;; *)

let draw_string s =
 let x = current_x ()
 and y = current_y () in
 add_command (Draw_string s);
 set_point (x + (String.length s * !x_text_size)) y;;

(* The font machinery *)
let current_font = ref default_font;;

let fonts = Hashtbl.create 11;;

let add_font f =
 if not (Hashtbl.mem fonts f) then Hashtbl.add fonts f f;;

let parse_font f =
  match f with
  | "Times" -> f, !x_text_size
  | "Times-Bold" -> f, !x_text_size
  | "Times-Italic" -> f, !x_text_size
  | "Times-BoldItalic" -> f, !x_text_size
  | "Times-Roman" -> f, !x_text_size
  | "Courrier" -> f, !x_text_size
  | "Courrier-Bold" -> f, !x_text_size
  | "Courrier-Italic" -> f, !x_text_size
  | "Courrier-BoldItalic" -> f, !x_text_size
  | "Helvetica" -> f, !x_text_size
  | "Helvetica-Bold" -> f, !x_text_size
  | "Helvetica-Italic" -> f, !x_text_size
  | "Helvetica-BoldItalic" -> f, !x_text_size
  | _ -> f, !x_text_size;;

let change_font f =
 let f, sz = parse_font f in
 add_font f;
 current_font := f;
 x_text_size := sz;
 sz;;

let set_font f =
 let sz = change_font f in
 add_command (Set_font (f, sz));;

let init_font () = ignore (change_font default_font);;

init_font ();;

let set_text_size i =
 x_text_size := i;
 y_text_size := i * 3 / 2;
 add_command (Set_font (!current_font, i));;

let text_size s = (String.length s * !x_text_size, !y_text_size);;

(* Images *)

type image;;

let transp = -1;;

(* Post Script functions embedded into the output *)
let ps_defs = "\
/m { moveto } def
/rm { rmoveto } def
/l { lineto } def
/c { currentpoint } def
/cp { currentlinewidth currentpoint } def
/dl { l c stroke m } def
/rl { rlineto c stroke m } def
/p { newpath m c lineto 1 setlinewidth stroke m setlinewidth} def
/pr  %% define the path for a rectangle w h x y
    { newpath moveto
      dup %% w h h
      0 exch rlineto %% rlineto 0 h -- w h
      exch 0 rlineto %% rlineto w 0 -- h
      0 exch sub 0 exch rlineto %% rlineto 0 -h --
      closepath } def
/dr %% w h x y
    { pr stroke moveto } def
/fr { pr fill moveto } def
/fc { 0 360 newpath arc fill moveto } def
/dc { 0 360 newpath arc stroke moveto } def
/ac { newpath m c } def
/fa { gsave translate scale newpath 0 0 m arc closepath fill grestore } def
/da { savematrix gsave translate scale newpath arc
      restorematrix stroke grestore } def
/fe { gsave translate scale newpath 0 0 1 0 360 arc fill grestore } def
/de { savematrix gsave translate scale newpath 0 0 1 0 360 arc
      restorematrix stroke grestore } def
/pc { newpath m} def
/dp { closepath stroke m} def
/fp { closepath fill m} def
/kcolor { 1 255 div } def
/color { kcolor mul } def
/stmp { /tmp exch def } def
/srgb {% r g b -
 color stmp color exch color exch tmp setrgbcolor 
} def
/t {
 show
} def
/savedmatrix [0 0 0 0 0 0] def
/savematrix {
 savedmatrix currentmatrix pop
} def
/restorematrix {
 savedmatrix setmatrix
} def
%% ISO fonts
/ISOLatin1Encoding [
 /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef
 /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef
 /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef
 /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef
 /space /exclam /quotedbl /numbersign /dollar /percent /ampersand /quoteright
 /parenleft /parenright /asterisk /plus /comma /minus /period /slash
 /zero /one /two /three /four /five /six /seven
 /eight /nine /colon /semicolon /less /equal /greater /question
 /at /A /B /C /D /E /F /G /H /I /J /K /L /M /N /O
 /P /Q /R /S /T /U /V /W /X /Y /Z /bracketleft /backslash /bracketright
                                                       /asciicircum /underscore
 /quoteleft /a /b /c /d /e /f /g /h /i /j /k /l /m /n /o
 /p /q /r /s /t /u /v /w /x /y /z /braceleft /bar /braceright /asciitilde
                                                                       /.notdef
 /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef
 /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef
 /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef
 /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef
 /space /exclamdown /cent /sterling /currency /yen /brokenbar /section
 /dieresis /copyright /ordfeminine /guillemotleft /logicalnot /hyphen
                                                            /registered /macron
 /degree /plusminus /twosuperior /threesuperior /acute /mu /paragraph
                                                                /periodcentered
 /cedilla /onesuperior /ordmasculine /guillemotright /onequarter /onehalf
                                                   /threequarters /questiondown
 /Agrave /Aacute /Acircumflex /Atilde /Adieresis /Aring /AE /Ccedilla
 /Egrave /Eacute /Ecircumflex /Edieresis /Igrave /Iacute /Icircumflex
                                                         /Idieresis
 /Eth /Ntilde /Ograve /Oacute /Ocircumflex /Otilde /Odieresis /multiply
 /Oslash /Ugrave /Uacute /Ucircumflex /Udieresis /Yacute /Thorn /germandbls
 /agrave /aacute /acircumflex /atilde /adieresis /aring /ae /ccedilla
 /egrave /eacute /ecircumflex /edieresis /igrave /iacute /icircumflex
                                                         /idieresis
 /eth /ntilde /ograve /oacute /ocircumflex /otilde /odieresis /divide
 /oslash /ugrave /uacute /ucircumflex /udieresis /yacute /thorn /ydieresis
] def
%% usage: isoname oldname makeisofont -
/makeisofont {
  dup findfont length dict dup begin
    exch findfont {
      exch dup /FID eq { pop pop } { exch def } ifelse
    } forall
    /Encoding ISOLatin1Encoding def
  end
  definefont pop
} def
";;

let make_iso_font f = sprintf
 "/ISO%s /%s makeisofont" f f;;

let hashtbl_assocs t =
 let res = ref [] in
 Hashtbl.iter (fun k v -> res := (k, v) :: !res) t;
 !res;;

let hashtbl_vals t =
 let res = ref [] in
 Hashtbl.iter (fun k v -> res := v :: !res) t;
 !res;;

let hashtbl_map f t =
 let res = ref [] in
 Hashtbl.iter (fun k v -> res := f k v :: !res) t;
 !res;;

let make_iso_fonts () =
 let font_list = hashtbl_vals fonts in
 String.concat "\n" (List.map make_iso_font font_list);;

let set_default_font default_font default_font_size = sprintf
 "/ISO%s findfont %i scalefont setfont\n" default_font default_font_size;;

let creator size_x size_y = sprintf
  "%%!PS-Adobe-2.0 EPSF-2.0\n\
   %%%%Creator: GraphPs\n\
   %%%%BoundingBox: 0 0 %i %i\n" size_x size_y;;

let document_fonts () =
  let font_list = hashtbl_vals fonts in
  sprintf
   "%%%%DocumentFonts: %s\n\
    %%%%EndComments\n" (String.concat " " font_list);;

let header size_x size_y =
  sprintf "%s%s" (creator size_x size_y) (document_fonts ());;

let prelude size_x size_y default_font default_font_size =
  sprintf
   "%s\
    %%BeginProcSet\n\
    gsave\n\
    %s\
    0 0 m\n\
    1 setlinewidth\n\
    1 setlinecap\n\
    2 setlinejoin\n\
    10 setmiterlimit\n\
    %s\n\
    %s\
    %%EndProcSet\n" (header size_x size_y)
        ps_defs
        (make_iso_fonts ())
        (set_default_font default_font default_font_size);;

(* Could be "showpage", if one wants automatic display of the current page. *)
let postlude () =
 if !eps_mode then "grestore\n" else "grestore\nshowpage\n%%end\n";;

let escape_char b = function
  | '(' | ')' | '\\' as c -> Buffer.add_char b '\\'; Buffer.add_char b c
  | c -> Buffer.add_char b c;;

let escape_char_for_ps c =
 let b = Buffer.create 3 in
 escape_char b c;
 Buffer.contents b;;

let escape_string_for_ps s =
 let l = String.length s in
 let b = Buffer.create l in
 for i = 0 to l - 1 do escape_char b s.[i] done;
 Buffer.contents b;;

type filling = Fill | Draw;;

let print_poly oc filling v =
  if Array.length v > 0 then begin
   let x, y = v.(0) in
   fprintf oc "c %i %i pc\n" x y;
   for i = 1 to Array.length v - 1 do
    let x, y = v.(i) in
    fprintf oc "%i %i l\n" x y
   done;
   if filling = Draw then fprintf oc "dp\n" else fprintf oc "fp\n"
  end;;

let to_ps oc = function
  | Open_graph (x, y) ->
     fprintf oc "%s" (prelude x y default_font default_font_size);
     set_color background;
     fill_rect 0 0 x y;
     set_color foreground
  | Close_graph ->
     fprintf oc "%s\n" (postlude ())
  | Set_rgb_color (r, g, b) ->
     fprintf oc "%i %i %i srgb\n" r g b
  | Plot (x, y) ->
     fprintf oc "cp %i %i p\n" x y
  | Moveto (x, y) ->
     fprintf oc "%i %i m\n" x y
  | Lineto (x, y) ->
     fprintf oc "%i %i dl\n" x y
  | Rmoveto (x, y) ->
     fprintf oc "%i %i rm\n" x y
  | Rlineto (x, y) ->
     fprintf oc "%i %i rl\n" x y
  | Set_line_width w ->
     fprintf oc "%i setlinewidth\n" w
  | Draw_char c ->
     fprintf oc "(%s) show\n" (escape_char_for_ps c)
  | Draw_string s ->
     fprintf oc "(%s) show\n" (escape_string_for_ps s)
  | Set_font (f, size) ->
     fprintf oc "/ISO%s findfont %i scalefont setfont\n" f (2 * size)
  | Draw_rect (x, y, w, h) ->
     fprintf oc "c %i %i %i %i dr\n" w h x y
  | Fill_rect (x, y, w, h) ->
     fprintf oc "c %i %i %i %i fr\n" w h x y
  | Fill_poly v ->
     print_poly oc Fill v
  | Draw_poly v ->
     print_poly oc Draw v
  | Draw_circle (x, y, r) ->
     fprintf oc "c %i %i %i dc\n" x y r
  | Fill_circle (x, y, r) ->
     fprintf oc "c %i %i %i fc\n" x y r
  | Draw_ellipse (x, y, rx, ry) ->
     fprintf oc "%i %i %i %i de\n" rx ry x y
  | Fill_ellipse (x, y, rx, ry) ->
     fprintf oc "%i %i %i %i fe\n" rx ry x y
  | Draw_arc (x, y, rx, ry, a1, a2) ->
     fprintf oc "0 0 1 %i %i %i %i %i %i da\n" a1 a2 rx ry x y
  | Fill_arc (x, y, rx, ry, a1, a2) ->
     fprintf oc "0 0 1 %i %i %i %i %i %i fa\n" a1 a2 rx ry x y
;;

let rev_opt l =
  let same_com com c =
    match com, c with
    | Set_rgb_color (r, g, b), Set_rgb_color (_, _, _) -> true
    | Set_line_width w, Set_line_width _ -> true
    | Set_font (f, size), Set_font (_, _) -> true
    | _, _ -> false in

  let rec opt_rec com accu l =
    match l with
    | [] -> com :: accu
    | c :: l ->
    match c with
    | Set_rgb_color (r, g, b) as c ->
       if same_com com c then opt_rec com accu l else opt_rec c (com :: accu) l
    | Moveto (x, y) ->
       begin match com with
       | Rmoveto (_, _) | Moveto (_, _) -> opt_rec com accu l
       | _ -> opt_rec c (com :: accu) l
       end
    | Rmoveto (x, y) ->
       begin match com with
       | Rmoveto (x0, y0) -> opt_rec (Rmoveto (x + x0, y + y0)) accu l
       | Moveto (x0, y0) -> opt_rec com accu l
       | _ -> opt_rec c (com :: accu) l
       end
    | Set_line_width w ->
       if same_com com c then opt_rec com accu l else opt_rec c (com :: accu) l
    | Set_font (f, size) ->
       if same_com com c then opt_rec com accu l else opt_rec c (com :: accu) l

    | Open_graph (_, _) | Close_graph
    | Plot (_, _)
    | Lineto (_, _) | Rlineto (_, _)
    | Draw_char _ | Draw_string _
    | Draw_rect (_, _, _, _)
    | Fill_rect (_, _, _, _) | Fill_poly _ | Draw_poly _
    | Draw_circle (_, _, _) | Fill_circle (_, _, _)
    | Draw_ellipse (_, _, _, _) | Fill_ellipse (_, _, _, _)
    | Draw_arc (_, _, _, _, _, _) | Fill_arc (_, _, _, _, _, _) ->
       opt_rec c (com :: accu) l in
  match l with
  | [] -> l
  | c :: l -> opt_rec c [] l;;

let change_bounding_box x y =
  current_ps_program := List.map (function
      Open_graph _ -> Open_graph (x, y)
    | c -> c
  ) !current_ps_program

let close_graph () =
 if !current_ps_program <> [] then begin
 let oc = !ps_out_channel in
 add_command Close_graph;
 let commands = !current_ps_program in
 List.iter (to_ps oc) (rev_opt commands);
 current_ps_program := [];
 flush oc;
 if oc != stdout then close_out oc end
;;

at_exit close_graph;;
