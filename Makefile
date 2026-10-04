# Makefile for the word-extract Emacs package.
#
#   make compile   byte-compile, treating every warning as an error
#   make test      run the ERT test suite in batch
#   make info      build the Info manual from the Texinfo source
#   make checkdoc  run checkdoc on the source
#   make all       compile, test, and build the manual
#   make clean     remove build products

EMACS   ?= emacs
PACKAGE  = word-extract
EL       = $(PACKAGE).el
ELC      = $(EL:.el=.elc)
TEXI     = $(PACKAGE).texi
INFO     = $(PACKAGE).info
TESTS    = test/$(PACKAGE)-tests.el

BATCH    = $(EMACS) -Q --batch -L .

.PHONY: all compile test info checkdoc clean

all: compile test info

compile: $(EL)
	$(BATCH) \
	  --eval "(setq byte-compile-error-on-warn t)" \
	  -f batch-byte-compile $(EL)

test:
	$(BATCH) -l $(EL) -l $(TESTS) -f ert-run-tests-batch-and-exit

info: $(INFO)

$(INFO): $(TEXI)
	makeinfo --no-split -o $@ $(TEXI)

checkdoc:
	$(BATCH) --eval "(checkdoc-file \"$(EL)\")"

clean:
	rm -f $(ELC) $(INFO)
