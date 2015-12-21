## global variables
# set parameters for inference 
# MODE=release 
# set parameters for debugging code
MODE=debug

# main commands
all: clean analysis manuscript

clean:
	rm -f *.aux *.bbl *.blg *.log *.pdf *.bak *~ *.Rout */*.Rout */*.pdf */*.aux */*.log *.rda */*.rda */*/*/.rda

# commands for generating manuscript
manuscript: article/article.pdf

article/article.pdf: article/article.Rmd article/Endnote_lib.bib article/preamble-latex.tex article/reference-style.csl
	R -e "MODE='$(MODE)';rmarkdown::render('article/article.Rmd')"

# commands for running analysis
analysis: results/results.rda

results/results.rda: results/.cache/09-*.rda R/analysis/10-*.R
	R CMD BATCH R/analysis/10-*.R --no-restore
	mv *.Rout results/.cache/

results/.cache/09-*.rda: results/.cache/08-*.rda R/analysis/09-*.R
	R CMD BATCH R/analysis/09-*.R --no-restore
	mv *.Rout results/.cache/

results/.cache/08-*.rda: results/.cache/07-*.rda R/analysis/08-*.R
	R CMD BATCH R/analysis/08-*.R --no-restore
	mv *.Rout results/.cache/

results/.cache/07-*.rda: results/.cache/06-*.rda R/analysis/07-*.R
	R CMD BATCH R/analysis/07-*.R --no-restore
	mv *.Rout results/.cache/

results/.cache/06-*.rda: results/.cache/05-*.rda R/analysis/06-*.R
	R CMD BATCH R/analysis/06-*.R --no-restore
	mv *.Rout results/.cache/

results/.cache/05-*.rda: results/.cache/04-*.rda R/analysis/05-*.R
	R CMD BATCH R/analysis/05-*.R --no-restore
	mv *.Rout results/.cache/

results/.cache/04-*.rda: results/.cache/03-*.rda R/analysis/04-*.R
	R CMD BATCH R/analysis/04-*.R --no-restore
	mv *.Rout results/.cache/

results/.cache/03-*.rda: results/.cache/02-*.rda R/analysis/03-*.R
	R CMD BATCH R/analysis/03-*.R --no-restore
	mv *.Rout results/.cache/

results/.cache/02-*.rda: results/.cache/01-*.rda R/analysis/02-*.R
	R CMD BATCH R/analysis/02-*.R --no-restore
	mv *.Rout results/.cache/

results/.cache/01-*.rda: results/.cache/00-*.rda R/analysis/01-*.R
	R CMD BATCH R/analysis/01-*.R --no-restore
	mv *.Rout results/.cache/

results/.cache/00-*.rda: R/analysis/00-*.R
	R CMD BATCH R/analysis/00-*.R --no-restore
	mv *.Rout results/.cache/


