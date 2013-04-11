/**************************************************************************/
/*                                                                        */
/*                               Flow Caml                                */
/*                                                                        */
/*          Vincent Simonet, Projet Cristal, INRIA Rocquencourt           */
/*                                                                        */
/*  Copyright 2002, 2003 Institut National de Recherche en Informatique   */
/*  et en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the Q Public License version 1.0.                  */
/*                                                                        */
/*  Author contact: Vincent.Simonet@inria.fr                              */
/*  Software page: http://cristal.inria.fr/~simonet/soft/flowcaml/        */
/*                                                                        */
/**************************************************************************/

/* $Id: myterminfo.c,v 1.2 2003/06/26 13:33:00 simonet Exp $ */

/* Read and output terminal commands */

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/callback.h>

#define HAS_TERMCAP


#if defined (HAS_TERMCAP) && !defined (NATIVE_CODE)

extern int tgetent (char * buffer, char * name);
extern char * tgetstr (char * id, char ** area);
extern int tgetnum (char * id);
extern int tputs (char * str, int count, int (*outchar)(int c));

CAMLprim value terminfo_getcodes (value unit)
{
  static char buffer[1024];
  char *term;
  static char area [1024];
  static char *area_p = area;
  static char *standout = NULL;
  static char *standend = NULL;
  CAMLparam1 (unit);
  CAMLlocal1 (result);

  result= alloc(2, 0);
  Store_field(result, 0, copy_string (""));
  Store_field(result, 1, copy_string (""));

  term = getenv ("TERM");
  if (term == NULL) return result;
  if (tgetent(buffer, term) != 1) return result;

  standout = tgetstr ("us", &area_p);
  standend = tgetstr ("ue", &area_p);
  if (standout == NULL || standend == NULL){
    standout = tgetstr ("so", &area_p);
    standend = tgetstr ("se", &area_p);
  }

  Store_field(result, 0, copy_string(standout));
  Store_field(result, 1, copy_string(standend));
  
  CAMLreturn (result);
  
}

#else /* defined (HAS_TERMCAP) && !defined (NATIVE_CODE) */

CAMLprim value terminfo_getcodes (value unit)
{
  CAMLparam1 (unit);
  CAMLlocal1 (result);

  result = alloc(2, 0);
  Store_field(result, 0, copy_string (""));
  Store_field(result, 1, copy_string (""));

  CAMLreturn (result);
}

#endif /* defined (HAS_TERMCAP) && !defined (NATIVE_CODE) */
