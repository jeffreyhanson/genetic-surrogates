## global variables
# set parameters for inference 
# MODE=release 
# set parameters for debugging code
MODE=debug

# main operations
all: clean analysis manuscript

clean:
	rm -f *.aux *.bbl *.blg *.log *.pdf *.bak *~ *.Rout */*.Rout */*.pdf */*.aux */*.log *.rda */*.rda */*/*.rda data/intermediate/*.rda data/intermediate/*.Rout
	rm data/intermediate/structure -rf
	rm data/intermediate/bayescan -rf
	rm code/rmarkdown/article_files/figure-latex/*.pdf -f
	rm code/rmarkdown/supporting_information_files/figure-latex/*.pdf -f
	rm code/rmarkdown/figures_files/figure-latex/*.pdf -f
	rm code/rmarkdown/figures.tex -f
	rm code/rmarkdown/figures.md -f
	rm code/rmarkdown/figures.docx -f
	rm code/rmarkdown/figures.pdf -f
	rm code/rmarkdown/supporting_information.tex -f
	rm code/rmarkdown/supporting_information.md -f
	rm code/rmarkdown/supporting_information.docx -f
	rm code/rmarkdown/supporting_information.pdf -f
	rm code/rmarkdown/article.md -f
	rm code/rmarkdown/article.tex -f
	rm code/rmarkdown/article.docx -f
	rm code/rmarkdown/article.pdf -f
	rm code/rmarkdown/tables.md -f
	rm code/rmarkdown/tables.tex -f
	rm code/rmarkdown/tables.docx -f
	rm code/rmarkdown/tables.pdf -f
	rm article/*.csv -f

pull_ms:
	git fetch
	git checkout $(git rev-parse HEAD) code/rmarkdown

# commands for generating manuscript
manuscript: article/article.pdf article/figures.pdf article/supporting_information.pdf article/tables.pdf

article/article.pdf: code/rmarkdown/article.Rmd code/rmarkdown/Endnote_lib.bib code/rmarkdown/preamble-latex.tex code/rmarkdown/reference-style.csl
	R -e "rmarkdown::render('code/rmarkdown/article.Rmd')"
	mv code/rmarkdown/article.pdf article/
	rm article/article.tex -f
	rm article/article.md -f

article/figures.pdf: code/rmarkdown/figures.Rmd code/rmarkdown/preamble-latex.tex code/rmarkdown/preamble-latex2.tex
	R -e "rmarkdown::render('code/rmarkdown/figures.Rmd')"
	mv code/rmarkdown/figures.pdf article/
	rm article/figures.tex -f
	rm article/figures.md -f

article/tables.pdf: code/rmarkdown/tables.Rmd code/rmarkdown/preamble-latex.tex code/rmarkdown/preamble-latex2.tex
	R -e "rmarkdown::render('code/rmarkdown/tables.Rmd')"
	mv code/rmarkdown/tables.pdf article/
	rm code/rmarkdown/tables.tex -f
	rm code/rmarkdown/tables.md -f

article/supporting_information.pdf: code/rmarkdown/supporting_information.Rmd code/rmarkdown/preamble-latex.tex code/rmarkdown/preamble-latex3.tex
	R -e "rmarkdown::render('code/rmarkdown/supporting_information.Rmd')"
	mv code/rmarkdown/supporting_information.pdf article/
	rm code/rmarkdown/supporting_information.tex -f
	rm code/rmarkdown/supporting_information.md -f

# commands for running analysis
analysis: data/final/results.rda

data/final/results.rda: data/intermediate/11-*.rda code/R/analysis/12-*.R
	R CMD BATCH --no-restore --no-save code/R/analysis/12-*.R
	mv *.Rout data/intermediate/

data/intermediate/11-*.rda: data/intermediate/07-*.rda data/intermediate/08-*.rda data/intermediate/09-*.rda data/intermediate/10-*.rda code/R/analysis/11-*.R
	R CMD BATCH --no-restore --no-save code/R/analysis/11-*.R
	mv *.Rout data/intermediate/

data/intermediate/10-*.rda: data/intermediate/06-*.rda code/R/analysis/10-*.R code/parameters/rapr.toml code/parameters/gurobi.toml
	R CMD BATCH --no-restore --no-save code/R/analysis/10-*.R
	mv *.Rout data/intermediate/

data/intermediate/09-*.rda: data/intermediate/06-*.rda code/R/analysis/09-*.R code/parameters/rapr.toml code/parameters/gurobi.toml
	R CMD BATCH --no-restore --no-save code/R/analysis/09-*.R
	mv *.Rout data/intermediate/

data/intermediate/08-*.rda: data/intermediate/06-*.rda code/R/analysis/08-*.R code/parameters/rapr.toml code/parameters/gurobi.toml
	R CMD BATCH --no-restore --no-save code/R/analysis/08-*.R
	mv *.Rout data/intermediate/

data/intermediate/07-*.rda: data/intermediate/06-*.rda code/R/analysis/07-*.R code/parameters/rapr.toml code/parameters/gurobi.toml
	R CMD BATCH --no-restore --no-save code/R/analysis/07-*.R
	mv *.Rout data/intermediate/

data/intermediate/06-*.rda: data/intermediate/05-*.rda code/R/analysis/06-*.R
	R CMD BATCH --no-restore --no-save code/R/analysis/06-*.R
	mv *.Rout data/intermediate/

data/intermediate/05-*.rda: data/intermediate/04-*.rda code/R/analysis/05-*.R code/parameters/nmds.toml
	R CMD BATCH --no-restore --no-save code/R/analysis/05-*.R
	mv *.Rout data/intermediate/

data/intermediate/04-*.rda: data/intermediate/03-*.rda code/R/analysis/04-*.R code/parameters/bayescan.toml code/parameters/pcadapt.toml
	R CMD BATCH --no-restore --no-save code/R/analysis/04-*.R
	mv *.Rout data/intermediate/

data/intermediate/03-*.rda: data/intermediate/02-*.rda code/R/analysis/03-*.R code/parameters/structure.toml
	R CMD BATCH --no-restore --no-save code/R/analysis/03-*.R
	mv *.Rout data/intermediate/

data/intermediate/02-*.rda: data/intermediate/01-*.rda code/R/analysis/02-*.R code/parameters/surrogate.toml
	R CMD BATCH --no-restore --no-save code/R/analysis/02-*.R
	mv *.Rout data/intermediate/

data/intermediate/01-*.rda: data/intermediate/00-*.rda code/R/analysis/01-*.R
	R CMD BATCH --no-restore --no-save code/R/analysis/01-*.R
	mv *.Rout data/intermediate/

data/intermediate/00-*.rda: code/R/analysis/00-*.R code/parameters/general.toml
	R CMD BATCH --no-restore --no-save '--args MODE=$(MODE)' code/R/analysis/00-*.R
	mv *.Rout data/intermediate/


