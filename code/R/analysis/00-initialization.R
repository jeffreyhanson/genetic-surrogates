#### set default error message
# parse args
args <- commandArgs(TRUE)
if (length(args)>0) {
	print(args)
	if (grepl('MODE',args))
		MODE <- strsplit(grep('MODE', args, value=TRUE), '=', fixed=TRUE)[[1]][[2]]
}

#### Load packages
# set checkpoint
if (!'checkpoint' %in% installed.packages()[,'Package']) install.packages('checkpoint')
if (!file.exists('~/.checkpoint')) dir.create('~/.checkpoint')
checkpoint::checkpoint('2016-08-03', R.version='3.3.1')
if (!'checkpoint' %in% installed.packages()[,'Package']) install.packages('checkpoint')

## load bioconductor packages
# install packages
if (!'qvalue' %in% installed.packages()[,'Package']) {
	source('http://bioconductor.org/biocLite.R')
	withr::with_libpaths(.libPaths()[1], biocLite('qvalue'))
}
library(qvalue)

# load CRAN packages
library(stats)
library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)
library(png)
library(withr)
library(Hmisc)
library(lme4)
library(multcomp)
library(data.table)
library(grid)
library(gridExtra)
library(plotrix)
library(plyr)
library(dplyr)
library(tidyr)
library(pander)
library(vegan)
library(rgeos)
library(rgdal)
library(sp)
library(raster)
library(testthat)
library(parallel)
library(cluster)
library(rworldxtra)
library(doParallel)
library(english)
library(session)
library(maptools)
library(RcppTOML)
library(optimx)
library(RColorBrewer)
library(mclust)
library(cluster)
library(RVAideMemoire)
library(MuMIn)
library(knitr)
library(pscl)
library(lazyWeave)
library(pcadapt)
library(broom)
library(raptr)

## load github packages
if (!'rticles' %in% installed.packages()[,'Package'])
	withr::with_libpaths(.libPaths()[1], devtools::install_github('cboettig/rticles', dependencies=TRUE))
library(rticles)

if (!'bayescanr' %in% installed.packages()[,'Package'])
	withr::with_libpaths(.libPaths()[1], devtools::install_github('jeffreyhanson/bayescanr', dependencies=TRUE))
library(bayescanr)

if (!'structurer' %in% installed.packages()[,'Package'])
	withr::with_libpaths(.libPaths()[1], devtools::install_github('jeffreyhanson/structurer', dependencies=TRUE))
library(structurer)

if (!'ResistanceGA' %in% installed.packages()[,'Package'])
	withr::with_libpaths(.libPaths()[1], devtools::install_github('wpeterman/ResistanceGA', dependencies=TRUE))
library(ResistanceGA)

if (!'rgurobi' %in% installed.packages()[,'Package'])
	withr::with_libpaths(.libPaths()[1], devtools::install_github('jeffreyhanson/rgurobi', dependencies=TRUE))
library(rgurobi)

if (!'gurobi' %in% installed.packages()[,'Package']) {
	# find gurobi R package
	gurobi.PTH <- dir('/opt', 'gurobi', full.names=TRUE)
	gurobi.PTH <- paste0(gurobi.PTH[length(gurobi.PTH)], '/linux64/R')
	gurobi.PTH <- dir(gurobi.PTH, 'gurobi', full.names=TRUE)[1]
	# install pkgs
	withr::with_libpaths(.libPaths()[1], install.packages('slam'))
	withr::with_libpaths(.libPaths()[1], install.packages(gurobi.PTH))
}

# manually install custom fork of ggplot2 for plotting
devtools::install_github('jeffreyhanson/ggplot2')
library(ggplot2)

### set parameters
if (!exists('MODE')) MODE <- 'debug'
cat('MODE = ',MODE,'\n')
general.params.LST <- RcppTOML::parseTOML('code/parameters/general.toml')

## misc settings
# set pander options
panderOptions('knitr.auto.asis', FALSE)

# set seed for reproducibility
set.seed(500)

# set default methods
select <- dplyr::select
rename<- dplyr::rename
extract <- raster::extract

### Load functions
for (x in dir(file.path('code', 'R', 'functions'), full.names=TRUE)) source(x)

# save workspace
save.session('data/intermediate/00-initialization.rda', compress='xz')

