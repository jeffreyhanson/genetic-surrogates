## global variables
# set parameters for inference 
MODE=release 
# set parameters for debugging code
# MODE=debug

# main operations
all: analysis manuscript

clean:
	@rm -f *.aux *.bbl *.blg *.log *.pdf *.bak *~ *.Rout */*.Rout */*.pdf */*.aux */*.log *.rda */*.rda */*/*.rda data/intermediate/*.rda data/intermediate/*.Rout
	@rm data/intermediate/structure -rf
	@rm data/intermediate/bayescan -rf
	@rm data/intermediate/pcadapt -rf
	@rm code/rmarkdown/article_files/figure-latex/*.pdf -f
	@rm code/rmarkdown/supporting_information_files/figure-latex/*.pdf -f
	@rm code/rmarkdown/supporting_information.tex -f
	@rm code/rmarkdown/supporting_information.md -f
	@rm code/rmarkdown/supporting_information.docx -f
	@rm code/rmarkdown/supporting_information.pdf -f
	@rm code/rmarkdown/article.md -f
	@rm code/rmarkdown/article.tex -f
	@rm code/rmarkdown/article.docx -f
	@rm code/rmarkdown/article.pdf -f
	@rm article/*.csv -f

pull_ms__gb:
	@git fetch
	@git checkout '$(COMMIT_ID)' code/rmarkdown

pull_ms:
	@scp -P 443 uqjhans4@cbcs-comp01.server.science.uq.edu.au:/home/uqjhans4/GitHub/genetic-surrogates/article/* article

push_ms:
	@scp -P 443 code/rmarkdown/* uqjhans4@cbcs-comp01.server.science.uq.edu.au:/home/uqjhans4/GitHub/genetic-surrogates/code/rmarkdown
	
# commands for generating manuscript
manuscript: article si

article: article/article.pdf article/article.docx

si: article/supporting_information.pdf

article/article.docx: code/rmarkdown/article.Rmd code/rmarkdown/references.bib code/rmarkdown/preamble.tex code/rmarkdown/reference-style.csl
	R -e "rmarkdown::render('code/rmarkdown/article.Rmd', output_file='article.docx')"
	mv code/rmarkdown/article.docx article/
	rm article/article.md -f
	rm article/article.tex -f
	rm article/article.utf8.md -f

article/article.pdf: code/rmarkdown/article.Rmd code/rmarkdown/references.bib code/rmarkdown/preamble.tex code/rmarkdown/reference-style.csl
	R -e "rmarkdown::render('code/rmarkdown/article.Rmd')"
	R -e "source('code/R/functions/format_pnas.R');format_pnas('code/rmarkdown/article.tex','code/rmarkdown/article.tex')"
	cd code/rmarkdown && pdflatex article.tex
	mv code/rmarkdown/article.pdf article/
	mv code/rmarkdown/article.tex article/
	rm article/article.md -f
	rm article/article.utf8.md -f

article/supporting_information.pdf: code/rmarkdown/supporting_information.Rmd code/rmarkdown/preamble.tex code/rmarkdown/si-preamble.tex
	R -e "rmarkdown::render('code/rmarkdown/supporting_information.Rmd')"
	mv code/rmarkdown/supporting_information.pdf article/
	rm code/rmarkdown/supporting_information.tex -f
	rm code/rmarkdown/supporting_information.md -f

# commands for running analysis
analysis: data/final/results.rda

data/final/results.rda: data/intermediate/12-*.rda code/R/analysis/13-*.R
	R CMD BATCH --no-restore --no-save code/R/analysis/13-*.R
	mv *.Rout data/intermediate/

data/intermediate/12-*.rda: data/intermediate/07-*.rda data/intermediate/08-*.rda data/intermediate/09-*.rda data/intermediate/10-*.rda data/intermediate/11-*.rda code/R/analysis/12-*.R
	R CMD BATCH --no-restore --no-save code/R/analysis/12-*.R
	mv *.Rout data/intermediate/

data/intermediate/11-*.rda: data/intermediate/06-*.rda code/R/analysis/11-*.R code/parameters/raptr.toml code/parameters/gurobi.toml
	R CMD BATCH --no-restore --no-save code/R/analysis/11-*.R
	mv *.Rout data/intermediate/

data/intermediate/10-*.rda: data/intermediate/06-*.rda code/R/analysis/10-*.R code/parameters/raptr.toml code/parameters/gurobi.toml
	R CMD BATCH --no-restore --no-save code/R/analysis/10-*.R
	mv *.Rout data/intermediate/

data/intermediate/09-*.rda: data/intermediate/06-*.rda code/R/analysis/09-*.R code/parameters/raptr.toml code/parameters/gurobi.toml
	R CMD BATCH --no-restore --no-save code/R/analysis/09-*.R
	mv *.Rout data/intermediate/

data/intermediate/08-*.rda: data/intermediate/06-*.rda code/R/analysis/08-*.R code/parameters/raptr.toml code/parameters/gurobi.toml
	R CMD BATCH --no-restore --no-save code/R/analysis/08-*.R
	mv *.Rout data/intermediate/
	
data/intermediate/07-*.rda: data/intermediate/06-*.rda code/R/analysis/07-*.R code/parameters/raptr.toml code/parameters/gurobi.toml
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

data/intermediate/03-*.rda: data/intermediate/02-*.rda code/R/analysis/03-*.R code/parameters/structure.toml code/parameters/clumpp.toml
	R CMD BATCH --no-restore --no-save code/R/analysis/03-*.R
	mv *.Rout data/intermediate/

data/intermediate/02-*.rda: data/intermediate/01-*.rda code/R/analysis/02-*.R code/parameters/surrogate.toml
	R CMD BATCH --no-restore --no-save code/R/analysis/02-*.R
	mv *.Rout data/intermediate/

data/intermediate/01-*.rda: data/intermediate/00-*.rda code/R/analysis/01-*.R
	R CMD BATCH --no-restore --no-save code/R/analysis/01-*.R
	mv *.Rout data/intermediate/

data/intermediate/00-*.rda: code/R/analysis/00-*.R code/parameters/general.toml data/raw/*
	R CMD BATCH --no-restore --no-save '--args MODE=$(MODE)' code/R/analysis/00-*.R
	mv *.Rout data/intermediate/



