# -*- mode:makefile-gmake; coding:utf-8; indent-tabs-mode:t; tab-width:4 -*-

NAME=cl-naive-store--documentation
PDFS=$(NAME).pdf
ORG_FILES := $(wildcard *.org)
all:documents
	@if [ $$(uname) = Darwin ] ; then open $(PDFS) ; else xpdf $(PDFS) ; fi

.PHONY:: documents pdfs

documents pdfs :$(PDFS)

cl-naive-store--documentation.pdf:index.pdf
	mv index.pdf cl-naive-store--documentation.pdf

index.pdf: $(ORG_FILES)
	-rm -f $@
	emacs --batch \
		--eval '(find-file "index.org")' \
		--funcall org-latex-export-to-pdf \
		--kill

# Generate PDF from org-mode document.
%.pdf:%.org
	-rm -f $@
	emacs --batch \
		--eval '(find-file "'$<'")' \
		--funcall org-latex-export-to-pdf \
		--kill
clean::
	- rm -f $(NAME).tex missfont.log ./--version.lock
