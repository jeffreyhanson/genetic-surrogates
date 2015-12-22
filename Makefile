## global variables
# set parameters for inference 
# MODE=release 
# set parameters for debugging code
MODE=debug

# main commands
all: clean analysis manuscript

clean:
	rm -f *.aux *.bbl *.blg *.log *.pdf *.bak *~ *.Rout */*.Rout */*.pdf */*.aux */*.log *.rda */*.rda results/.cache/*.rda results/.cache/*.Rout

# commands for generating manuscript
manuscript: article/article.pdf

article/article.pdf: article/article.Rmd article/Endnote_lib.bib article/preamble-latex.tex article/reference-style.csl
	R -e "MODE='$(MODE)';rmarkdown::render('article/article.Rmd')"

# commands for running analysis
analysis: results/results.rda

results/results.rda: results/.cache/09-*.rda R/analysis/10-*.R
	R CMD BATCH --no-restore --no-save R/analysis/10-*.R
	mv *.Rout results/.cache/

results/.cache/09-*.rda: results/.cache/08-*.rda R/analysis/09-*.R
	R CMD BATCH --no-restore --no-save R/analysis/09-*.R
	mv *.Rout results/.cache/

results/.cache/08-*.rda: results/.cache/07-*.rda R/analysis/08-*.R
	R CMD BATCH --no-restore --no-save R/analysis/08-*.R
	mv *.Rout results/.cache/

results/.cache/07-*.rda: results/.cache/06-*.rda R/analysis/07-*.R
	R CMD BATCH --no-restore --no-save R/analysis/07-*.R
	mv *.Rout results/.cache/

results/.cache/06-*.rda: results/.cache/05-*.rda R/analysis/06-*.R
	R CMD BATCH --no-restore --no-save R/analysis/06-*.R
	mv *.Rout results/.cache/

results/.cache/05-*.rda: results/.cache/04-*.rda R/analysis/05-*.R
	R CMD BATCH --no-restore --no-save R/analysis/05-*.R
	mv *.Rout results/.cache/

results/.cache/04-*.rda: results/.cache/03-*.rda R/analysis/04-*.R
	R CMD BATCH --no-restore --no-save R/analysis/04-*.R
	mv *.Rout results/.cache/

results/.cache/03-*.rda: results/.cache/02-*.rda R/analysis/03-*.R
	R CMD BATCH --no-restore --no-save R/analysis/03-*.R
	mv *.Rout results/.cache/

results/.cache/02-*.rda: results/.cache/01-*.rda R/analysis/02-*.R
	R CMD BATCH --no-restore --no-save R/analysis/02-*.R
	mv *.Rout results/.cache/

results/.cache/01-*.rda: results/.cache/00-*.rda R/analysis/01-*.R
	R CMD BATCH --no-restore --no-save R/analysis/01-*.R
	mv *.Rout results/.cache/

results/.cache/00-*.rda: R/analysis/00-*.R
	R CMD BATCH --no-restore --no-save '--args MODE=$(MODE)' R/analysis/00-*.R
	mv *.Rout results/.cache/


