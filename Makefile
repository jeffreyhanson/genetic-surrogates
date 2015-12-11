MODE=debug

all: clean results pdf

pdf: article/article.pdf

article/article.pdf: article/article.Rmd article/Endnote_lib.bib article/preamble-latex.tex article/reference-style.csl
	R -e "MODE='$(MODE)';rmarkdown::render('article/article.Rmd')"

results: results/*.rda

results/*.rda: R/analysis/*.R R/functions/*.R
	echo "$(MODE)"
	R -e "MODE='$(MODE)';for(i in dir('R/analysis', '^.*.R', full=TRUE)){cat(basename(i),'\n');source(i)}"

clean:
	rm -f *.aux *.bbl *.blg *.log *.pdf *.bak *~ *.Rout */*.Rout */*.pdf */*.aux */*.log *.rda */*.rda

