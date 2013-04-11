##########################################################################
#                                                                        #
#                                  Dalton                                #
#                      an efficient implementation of                    #
#                 type inference with structural subtyping               #
#                                                                        #
#          Vincent Simonet, Projet Cristal, INRIA Rocquencourt           #
#                                                                        #
#  Copyright 2002, 2003 Institut National de Recherche en Informatique   #
#  et en Automatique.  All rights reserved.  This file is distributed    #
#  under the terms of the GNU Library General Public License, with the   #
#  special exception on linking described in file LICENSE.               #
#                                                                        #
#  Author contact: Vincent.Simonet@inria.fr                              #
#  Software page: http://cristal.inria.fr/~simonet/soft/dalton/          #
#                                                                        #
##########################################################################

# $Id: Makefile,v 1.2 2003/06/26 13:32:46 simonet Exp $



AVERELL.PATH= ../src-averell

include ../Makefile.common
include $(AVERELL.PATH)/Makefile.defs
include Makefile.defs
include Makefile.config

SRCDIRS= . $(AVERELL.PATH)



##########################################################################

all: dalton.cma 
opt: dalton.cmxa

install:
	mkdir -p $(INSTALLDIR)
	cp $(DALTON.INTERFACES) $(INSTALLDIR)
	cp dalton.cma $(INSTALLDIR)
	$(call copyifexists,dalton.cmxa,$(INSTALLDIR))
	$(call copyifexists,dalton.a,$(INSTALLDIR))

clean::
	rm -f config.cache config.log config.status Makefile.config
	rm -f ../Makefile.common

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



##########################################################################

CMO= $(AVERELL.CMO:%=$(AVERELL.PATH)/%) $(DALTON.CMO)
CMX= $(AVERELL.CMX:%=$(AVERELL.PATH)/%) $(DALTON.CMX)

dalton.cma: $(CMO)
	$(OCAMLC) $(OCAMLC_FLAGS) -a -o $@ $(CMO)

dalton.cmxa: $(CMX)
	$(OCAMLOPT) $(OCAMLOPT_FLAGS) -a -o $@ $(CMX)



include Depend
