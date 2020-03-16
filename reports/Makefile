# Makefile
#  Build the LaTeX files defined by latexfile.
#
# Dakota Alton

latexfile = main
LATEX = /usr/local/apps/tex_live/current/bin/x86_64-linux/pdflatex
BIBTEX = /usr/local/apps/tex_live/current/bin/x86_64-linux/bibtex
DVIPS = /usr/local/apps/tex_live/current/bin/x86_64-linux/dvips

$(latexfile).pdf : $(latexfile).ps
	ps2pdf $^ $@

$(latexfile).ps : $(latexfile).dvi
	$(DVIPS) $^

$(latexfile).dvi : $(latexfile).tex
	#$(LATEX) $^
	#$(BIBTEX) $(<:%.tex=%)
	# run a second time to generate bib and toc
	$(LATEX) $^
	$(LATEX) $^

.PHONY : clean

clean:
	rm -rf *.pdf *.ps *.dvi *.out *.log *.aux *.bbl *.blg *.pyg *.toc
