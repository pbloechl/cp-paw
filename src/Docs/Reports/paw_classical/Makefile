
#latex article.tex (this will generate a document with question marks
#   in place of unknown references)
#   ->aux,toc,idx,log,out,dvi
#
# bibtex article (this will parse all the .bib files that were
#   included in the article and generate metainformation regarding
#   references)
#   -> bbl,blg
#
# latex article.tex (this will generate document with all the
# references in the correct places)
#
# latex article.tex (just in case if adding references broke page
# numbering somewhere)
#
#; RC=$? if (($RC != 0 )); then; echo 'error from latex'; exit 0 ; fi 
#
export NAME=paw_classical

pdf : ${NAME}.pdf 

ps : ${NAME}.ps 

all : ${NAME}.pdf ${NAME}.ps 

clean : 
	rm ${NAME}.aux ${NAME}.bbl ${NAME}.blg ${NAME}.idx \
           ${NAME}.ilg ${NAME}.ind ${NAME}.log ${NAME}.out \
           ${NAME}.toc ${NAME}.dvi ${NAME}.ps ${NAME}.pdf

%.dvi : %.tex 
	latex $*.tex
	bibtex $*
	latex $*.tex
	makeindex $*.idx
	latex $*.tex

%.pdf : %.dvi
	dvipdf $*.dvi

%.ps : %.dvi
	dvips $*.dvi




