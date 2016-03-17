## global variables
# set parameters for inference 
# MODE=release 
# set parameters for debugging code
MODE=debug

# main operations
all: clean analysis manuscript

clean:
	rm -f *.aux *.bbl *.blg *.log *.pdf *.bak *~ *.Rout */*.Rout */*.pdf */*.aux */*.log *.rda */*.rda results/.cache/*.rda results/.cache/*.Rout
	rm results/.cache/structure -rf
	rm results/.cache/bayescan -rf
	rm article/article_files/figure-latex/*.pdf -f
	rm article/supporting_information_files/figure-latex/*.pdf -f
	rm article/figures_files/figure-latex/*.pdf -f
	rm article/figures.tex -f
	rm article/figures.md -f
	rm article/figures.docx -f
	rm article/supporting_information.tex -f
	rm article/supporting_information.md -f
	rm article/supporting_information.docx -f
	rm article/article.md -f
	rm article/article.tex -f
	rm article/article.docx -f
	
# commands for generating manuscript
manuscript: article/article.pdf

article/article.pdf: article/article.Rmd article/Endnote_lib.bib article/preamble-latex.tex article/reference-style.csl article/supporting_information.pdf article/figures.pdf
	R -e "rmarkdown::render('article/article.Rmd')"
	pandoc -s article/article.tex -o article/article.docx
	rm article/article.tex -f
	rm article/article.md -f

article/figures.pdf: article/figures.Rmd article/preamble-latex.tex article/preamble-latex2.tex
	R -e "rmarkdown::render('article/figures.Rmd')"
	pandoc -s article/figures.tex -o article/figures.docx
	rm article/figures.tex -f
	rm article/figures.md -f

article/supporting_information.pdf: article/supporting_information.Rmd article/preamble-latex.tex article/preamble-latex3.tex
	R -e "rmarkdown::render('article/supporting_information.Rmd')"
	pandoc -s article/supporting_information.tex -o article/supporting_information.docx
	rm article/supporting_information.tex -f
	rm article/supporting_information.md -f

# commands for running analysis
analysis: results/results.rda

results/results.rda: results/.cache/11-*.rda R/analysis/12-*.R
	R CMD BATCH --no-restore --no-save R/analysis/12-*.R
	mv *.Rout results/.cache/

results/.cache/11-*.rda: results/.cache/07-*.rda results/.cache/08-*.rda results/.cache/09-*.rda results/.cache/10-*.rda R/analysis/11-*.R
	R CMD BATCH --no-restore --no-save R/analysis/11-*.R
	mv *.Rout results/.cache/

results/.cache/10-*.rda: results/.cache/06-*.rda R/analysis/10-*.R parameters/rapr.toml parameters/gurobi.toml
	R CMD BATCH --no-restore --no-save R/analysis/10-*.R
	mv *.Rout results/.cache/

results/.cache/09-*.rda: results/.cache/06-*.rda R/analysis/09-*.R parameters/rapr.toml parameters/gurobi.toml
	R CMD BATCH --no-restore --no-save R/analysis/09-*.R
	mv *.Rout results/.cache/

results/.cache/08-*.rda: results/.cache/06-*.rda R/analysis/08-*.R parameters/rapr.toml parameters/gurobi.toml
	R CMD BATCH --no-restore --no-save R/analysis/08-*.R
	mv *.Rout results/.cache/

results/.cache/07-*.rda: results/.cache/06-*.rda R/analysis/07-*.R parameters/rapr.toml parameters/gurobi.toml
	R CMD BATCH --no-restore --no-save R/analysis/07-*.R
	mv *.Rout results/.cache/

results/.cache/06-*.rda: results/.cache/05-*.rda R/analysis/06-*.R
	R CMD BATCH --no-restore --no-save R/analysis/06-*.R
	mv *.Rout results/.cache/

results/.cache/05-*.rda: results/.cache/04-*.rda R/analysis/05-*.R parameters/nmds.toml
	R CMD BATCH --no-restore --no-save R/analysis/05-*.R
	mv *.Rout results/.cache/

results/.cache/04-*.rda: results/.cache/03-*.rda R/analysis/04-*.R parameters/bayescan.toml
	R CMD BATCH --no-restore --no-save R/analysis/04-*.R
	mv *.Rout results/.cache/

results/.cache/03-*.rda: results/.cache/02-*.rda R/analysis/03-*.R parameters/mclust.toml parameters/nmds.toml
	R CMD BATCH --no-restore --no-save R/analysis/03-*.R
	mv *.Rout results/.cache/

results/.cache/02-*.rda: results/.cache/01-*.rda R/analysis/02-*.R
	R CMD BATCH --no-restore --no-save R/analysis/02-*.R
	mv *.Rout results/.cache/

results/.cache/01-*.rda: results/.cache/00-*.rda R/analysis/01-*.R
	R CMD BATCH --no-restore --no-save R/analysis/01-*.R
	mv *.Rout results/.cache/

results/.cache/00-*.rda: R/analysis/00-*.R parameters/general.toml
	R CMD BATCH --no-restore --no-save '--args MODE=$(MODE)' R/analysis/00-*.R
	mv *.Rout results/.cache/


