##########################################################################
#                                                                        #
#                               Flow Caml                                #
#                                                                        #
#          Vincent Simonet, Projet Cristal, INRIA Rocquencourt           #
#                                                                        #
#  Copyright 2002, 2003 Institut National de Recherche en Informatique   #
#  et en Automatique.  All rights reserved.  This file is distributed    #
#  under the terms of the Q Public License version 1.0.                  #
#                                                                        #
#  Author contact: Vincent.Simonet@inria.fr                              #
#  Software page: http://cristal.inria.fr/~simonet/soft/flowcaml/        #
#                                                                        #
##########################################################################

# $Id: Makefile,v 1.8 2003/09/23 14:46:40 simonet Exp $



AVERELL.PATH=../src-averell
DALTON.PATH=../src-dalton

include ../Makefile.common
include $(AVERELL.PATH)/Makefile.defs
include $(DALTON.PATH)/Makefile.defs
include Makefile.defs
include Makefile.config

SRCDIRS= $(AVERELL.PATH) $(DALTON.PATH) $(FLOWCAMLDIRS)
BYTEBIN=flowcaml$(EXE) flowcamlc$(EXE) flowcamlpol$(EXE) flowcamldep$(EXE) \
 flowcamlmerge$(EXE) flowcamldoc$(EXE)
OPTBIN=flowcaml.opt$(EXE) flowcamlc.opt$(EXE)



##########################################################################

all: $(BYTEBIN)

opt: $(OPTBIN)

stdlib: 
	cd stdlib && $(MAKE)

runlib:
	cd runlib && $(MAKE)

world:
	$(MAKE) all
ifdef OCAMLOPT
	$(MAKE) opt
endif
	$(MAKE) stdlib
	$(MAKE) runlib
	@echo
	@echo ------------------------------------------------------------
	@echo Compilation seems to succeed.
	@echo You can now install the software.  As superuser, do
	@echo \'make install\'.
	@echo ------------------------------------------------------------


install:
	if test -d $(INSTALLDIR_BIN); then : ; \
	else mkdir -p $(INSTALLDIR_BIN); fi
	if test -d $(INSTALLDIR_STDLIB); then : ; \
	else mkdir -p $(INSTALLDIR_STDLIB); fi
	if test -d $(INSTALLDIR_RUNLIB); then : ; \
	else mkdir -p $(INSTALLDIR_RUNLIB); fi

ifdef OCAMLOPT
	cp -f flowcaml.opt$(EXE) $(INSTALLDIR_BIN)/flowcaml$(EXE)
	cp -f flowcamlc.opt$(EXE) $(INSTALLDIR_BIN)/flowcamlc$(EXE)
else
	cp -f flowcaml$(EXE) $(INSTALLDIR_BIN)/flowcaml$(EXE)
	cp -f flowcamlc$(EXE) $(INSTALLDIR_BIN)/flowcamlc$(EXE)
endif
	cp -f flowcamldep$(EXE) $(INSTALLDIR_BIN)
	cp -f flowcamlpol$(EXE) $(INSTALLDIR_BIN)
	cd stdlib && cp *.fcmi $(INSTALLDIR_STDLIB)
	cd runlib && cp *.mli *.cmi *.cma *.cmxa *.a $(INSTALLDIR_RUNLIB)

	@echo
	@echo ------------------------------------------------------------
	@echo Installation seems to succeed.  
	@echo 
	@echo If you want to uninstall flowcaml from your system do
	@echo \'make uninstall\' as superuser.
	@echo ------------------------------------------------------------

uninstall:
	$(foreach f,$(BYTEBIN) $(OPTBIN),\
	  if test -f $(INSTALLDIR_BIN)/$(f); then rm $(INSTALLDIR_BIN)/$(f); fi;)
	rm -rf $(INSTALLDIR_STDLIB)
	rm -rf $(INSTALLDIR_RUNLIB)



##########################################################################

flowcaml$(EXE): $(FLOWCAML.CMO)
	$(OCAMLC) $(OCAMLC_LINK_FLAGS) -o $@ $(CORELIBS.CMA) $(FLOWCAML.CMO)

flowcaml.opt$(EXE): $(FLOWCAML.CMX)
	$(OCAMLOPT) -o $@ $(CORELIBS.CMXA) $(FLOWCAML.CMX)

flowcamlc$(EXE): $(FLOWCAMLC.CMO)
	$(OCAMLC) $(OCAMLC_LINK_FLAGS) -o $@ $(CORELIBS.CMA) $(FLOWCAMLC.CMO)

flowcamlc.opt$(EXE): $(FLOWCAMLC.CMX)
	$(OCAMLOPT) -o $@ $(CORELIBS.CMXA) $(FLOWCAMLC.CMX)

flowcamldep$(EXE): $(FLOWCAMLDEP.CMO)
	$(OCAMLC) $(OCAMLC_LINK_FLAGS) -o $@ $(FLOWCAMLDEP.CMO)

flowcamlmerge$(EXE): $(FLOWCAMLMERGE.CMO)
	$(OCAMLC) $(OCAMLC_LINK_FLAGS) -o $@ $(CORELIBS.CMA) $(FLOWCAMLMERGE.CMO)

flowcamldoc$(EXE): $(FLOWCAMLDOC.CMO)
	$(OCAMLC) $(OCAMLC_LINK_FLAGS) -o $@ $(CORELIBS.CMA) $(FLOWCAMLDOC.CMO)

flowcamlpol$(EXE): $(FLOWCAMLPOL.CMO)
	$(OCAMLC) $(OCAMLC_LINK_FLAGS) -o $@ $(CORELIBS.CMA) $(FLOWCAMLPOL.CMO)

#########################################################################

FLOWCAML-GENERATED= parsing/linenum.ml parsing/lexer.ml \
  parsing/parser.mli parsing/parser.ml utils/config.ml \
  docgen/doc_parser.mli docgen/doc_parser.ml docgen/doc_lexer.ml \
  $(AVERELL.PATH)/avl_graphics.ml

ifdef OCAML_GRAPHICS
$(AVERELL.PATH)/avl_graphics.ml:$(AVERELL.PATH)/avl_graphics.mlp
	cpp -DGRAPHICS -P $< > $@
else
$(AVERELL.PATH)/avl_graphics.ml:$(AVERELL.PATH)/avl_graphics.mlp
	cpp -P $< > $@
endif

depend: $(FLOWCAML-GENERATED)

clean::
	rm -f $(FLOWCAML-GENERATED)
	rm -f $(BYTEBIN) $(OPTBIN)
	cd stdlib && $(MAKE) clean
	cd runlib && $(MAKE) clean
	rm -f Makefile.config ../Makefile.common
	rm -f config.cache config.log config.status



#########################################################################

DUMP=ocamlprof.dump

profiling:
	rm -rf profiling
	mkdir profiling
	for d in $(SRCDIRS); do \
	  mkdir profiling/$$d; \
	  for f in $$d/*.ml; do \
	    echo $$f; \
	    ocamlprof -f $(DUMP) $$f > profiling/$$f; \
          done; \
        done

#########################################################################

.PHONY : all opt stdlib runlib world profiling install uninstall clean depend

include Depend
