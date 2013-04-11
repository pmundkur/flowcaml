# $Id: Makefile,v 1.1 2003/05/13 13:56:16 simonet Exp $

# Files to install
FILES=flowcaml.el

# Where to install. If empty, automatically determined.
#EMACSDIR=

# Name of Emacs executable
EMACS=emacs

# Command for byte-compiling the files
COMPILECMD=(progn \
              (setq load-path (cons "." load-path)) \
              (byte-compile-file "flowcaml.el"))

install:
	@if test "$(EMACSDIR)" = ""; then \
          set xxx `($(EMACS) --batch --eval "(mapcar 'print load-path)") \
                   2>/dev/null | \
                   sed -n -e '/\/site-lisp/s/"//gp'`; \
          if test "$$2" = ""; then \
            echo "Cannot determine Emacs site-lisp directory"; \
            exit 2; \
          fi; \
          $(MAKE) EMACSDIR="$$2" simple-install; \
        else \
          $(MAKE) simple-install; \
        fi

simple-install:
	@echo "Installing in $(EMACSDIR)..."
	if test -d $(EMACSDIR); then : ; else mkdir -p $(EMACSDIR); fi
	cp $(FILES) $(EMACSDIR)
	cd $(EMACSDIR); $(EMACS) --batch --eval '$(COMPILECMD)'
