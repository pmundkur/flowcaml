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

# $Id: Makefile,v 1.5 2003/06/26 13:32:51 simonet Exp $



##########################################################################
# Configuration

include ../../Makefile.common
SRCDIRS= .

UNITS=flowperv charray fmarshal

CMI=$(UNITS:%=%.cmi)
CMO=$(UNITS:%=%.cmo)
CMX=$(UNITS:%=%.cmx)

all: $(CMI) flowcamlrun.cma
ifdef OCAMLOPT
all: flowcamlrun.cmxa
endif

clean::
	rm -f charray.mli fmarshal.mli

flowcamlrun.cma: $(CMI) $(CMO)
	$(OCAMLC) -o $@ -a $(CMO)

flowcamlrun.cmxa: $(CMI) $(CMX)
	$(OCAMLOPT) -o $@ -a $(CMX)

%.mli:
	cp -f ../stdlib/$@ $@

