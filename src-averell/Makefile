##########################################################################
#                                                                        #
#                                 Averell                                #
#                                                                        #
#          Vincent Simonet, Projet Cristal, INRIA Rocquencourt           #
#                                                                        #
#  Copyright 2002, 2003 Institut National de Recherche en Informatique   #
#  et en Automatique.  All rights reserved.  This file is distributed    #
#  under the terms of the GNU Library General Public License, with the   #
#  special exception on linking described in file LICENSE.               #
#                                                                        #
#  Author contact: Vincent.Simonet@inria.fr                              #
#  Software page: http://cristal.inria.fr/~simonet/soft/                 #
#                                                                        #
##########################################################################

# $Id: Makefile,v 1.4 2003/06/30 18:05:31 simonet Exp $



include ../Makefile.common
include Makefile.defs
include Makefile.config

SRCDIRS= .

FLOWCAML-GENERATED=avl_graphics.ml


##########################################################################

all: averell.cma
opt: averell.cmxa

install:
	mkdir -p $(INSTALLDIR)
	cp $(AVERELL.INTERFACES) $(INSTALLDIR)
	cp averell.cma $(INSTALLDIR)
	$(call copyifexists,averell.cmxa,$(INSTALLDIR))
	$(call copyifexists,averell.a,$(INSTALLDIR))

clean::
	rm -f config.cache config.log config.status Makefile.config
	rm -f ../Makefile.common
	rm -f demo

world:
	$(MAKE) all
ifdef OCAMLOPT
	$(MAKE) opt
endif
	@echo
	@echo ------------------------------------------------------------
	@echo Compilation seems to succeed.
	@echo You can now install the software.  As superuser, do
	@echo \'make install\'.
	@echo ------------------------------------------------------------


#########################################################################

demo: averell.cma demo.cmo
	$(OCAMLC) -o $@ unix.cma graphics.cma $^


#########################################################################

averell.cma: $(AVERELL.CMO)
	$(OCAMLC) $(OCAMLC_FLAGS) -a -o $@ $(AVERELL.CMO)

averell.cmxa: $(AVERELL.CMX)
	$(OCAMLOPT) $(OCAMLOPT_FLAGS) -a -o $@ $(AVERELL.CMX)

include Depend
