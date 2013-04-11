##########################################################################
#                                                                        #
#                               Flow Caml                                #
#                                                                        #
#          Vincent Simonet, Projet Cristal, INRIA Rocquencourt           #
#                                                                        #
#  Copyright 2002, 2003 Institut National de Recherche en Informatique   #
#  et en Automatique.  All rights reserved.  This file is distributed    #
#  under the terms of the GNU Library General Public License, with the   #
#  special exception on linking described in file ../LICENSE.            #
#                                                                        #
#  Author contact: Vincent.Simonet@inria.fr                              #
#  Software page: http://cristal.inria.fr/~simonet/soft/flowcaml/        #
#                                                                        #
##########################################################################

# $Id: Makefile,v 1.10 2003/06/26 13:32:51 simonet Exp $



##########################################################################
# Units

units-implemented= \
  array \
  buffer \
  char \
  complex \
  hashtbl \
  list \
  map \
  queue \
  random \
  set \
  stack

units-not-implemented= \
  charray \
  digest \
  filename \
  fmarshal \
  int32 \
  int64 \
  nativeint \
  string \
  sys

MLI= $(units-implemented:%=%.mli) $(units-not-implemented:%=%.mli)
ML= $(units-implemented:%=%.ml)


##########################################################################
# Configuration

FLOWCAMLC= $(shell if test -f "../flowcamlc.opt"; \
                   then echo "../flowcamlc.opt"; \
                   else echo "../flowcamlc"; fi)\
	   -nostdlib

FLOWCAMLDEP= ../flowcamldep



##########################################################################

all: pervasives.fcmi $(MLI) $(ML)

clean:
	rm -f *.fcmi *.ml *.mli *.cmo *.cmi
	rm -Rf doc

depend:
	$(FLOWCAMLDEP) *.fml *.fmli > Depend

pervasives.fcmi: pervasives.fmli
	$(FLOWCAMLC) -nopervasives $<



##########################################################################
# Documentation

FLOWCAMLDOC=../flowcamldoc
DOCDIR=./doc

.PHONY: doc
doc:
	mkdir -p doc
	for i in *.fmli; do \
	echo $$i; \
	$(FLOWCAMLDOC) -i -d $(DOCDIR) $$i; \
	done


##########################################################################
# Generic rules

.SUFFIXES: .fml .fmli .fcmi .ml .mli

.fmli.mli: pervasives.fcmi
	$(FLOWCAMLC) $(COMPFLAGS) $<

.fml.ml: pervasives.fcmi
	$(FLOWCAMLC) $(COMPFLAGS) $<



include Depend
