#### set default error message
options(error=function(){
  save.image(paste0('results-error-',as.character(Sys.time()),'.rda'))
  traceback()
})

# parse args
args <- commandArgs(TRUE)
if (length(args)>1) {
	print(args)
	if (grepl('MODE',args))
		MODE <- strsplit(grep('MODE', args, value=TRUE), '=', fixed=TRUE)[[1]][[2]]
}
	
	
#### Load pacakges
# load CRAN packages
suppressMessages(library(stats))
suppressMessages(library(data.table))
suppressMessages(library(ggplot2))
suppressMessages(library(grid))
suppressMessages(library(gridExtra))
suppressMessages(library(plotrix))
suppressMessages(library(plyr))
suppressMessages(library(dplyr))
suppressMessages(library(tidyr))
suppressMessages(library(pander))
suppressMessages(library(multcomp))
suppressMessages(library(vegan))
suppressMessages(library(rgeos))
suppressMessages(library(testthat))
suppressMessages(library(parallel))
suppressMessages(library(cluster))
suppressMessages(library(rworldxtra))
suppressMessages(library(snow))
suppressMessages(library(doParallel))
suppressMessages(library(english))
suppressMessages(library(session))
suppressMessages(library(maptools))

## load github packages
# devtools::install_github('paleo13/raspr')
suppressMessages(library(rapr))
# devtools::install_github('paleo13/bayescanr')
suppressMessages(library(bayescanr))
# devtools::install_github('paleo13/structurer')
suppressMessages(library(structurer))

# set pander options
panderOptions('knitr.auto.asis', FALSE)

# set seed for reproducibility
set.seed(500)

# set default select method
select <- dplyr::select

### Load functions
paths <- c(
	'genetic.surrogates-internal.R',
	'data.R',
	'misc.R',
	'BayeScanResults.R',
	'generics.R',
	'BayeScanData.R',
	'BayeScanOpts.R',
	'BayeScan.R',
	'extractResults.R',
	'format.table.R',
	'make.multi.species.AttributeSpace.R',
	'make.single.species.AttributeSpace.R',
	'make.targets.R',
	'species.prioritisation.R'
)
for (x in dir(file.path('R', 'functions'), full.names=TRUE)) source(x)

### set parameters
if (!exists('MODE')) MODE <- 'debug'
cat('MODE = ',MODE,'\n')

if (MODE=='debug') {
	## debugging parameters
	# number of species
	n.spp <- 27
	# structure parameters
	st.threads <- 1
	st.numruns <- 4
	st.k <- 1:3
	st.numreps <- 10
	st.burnin <- 10
	st.noadmix  <- FALSE
	st.admburnin  <- 10
	st.probthresh <- 0.75
	# clumpp parameters
	cl.repeats <- 1000
	cl.s <- FALSE
	cl.m <- 'LargeKGreedy'
	# BayeScan parameters
	bs.reps <- 2
	bs.fdr <- 0.8
	bs.threads <- 1
	bs.n <- 10
	bs.thin <- 1
	bs.nbp <- 3
	bs.pilot <- 3
	bs.burn <- 40
	bs.freq <- 0.05
	# MDS parameters
	mds.k <- 2
	mds.trymax <- 2
	# Gurobi parameters
	gb.Threads <- 1L
	gb.MIPGap <- 0.9
	# targets
	rapr.amount.target <- 0.2
	rapr.surrogate.target <- 0.9
	rapr.genetic.target <- 0.9
	rapr.pareto.surrogate.targets <- seq(0.01, 1, length.out=3)
}

if (MODE=='release') {
	## analysis parameters
	# number of species
	n.spp <- 27
	# structure parameters
	st.threads <- 10
	st.numruns <- 20
	st.k <- 1:10
	st.numreps <- 100000
	st.burnin <- 100000
	st.noadmix  <- FALSE
	st.admburnin  <- 500
	st.probthresh <- 0.75
	# clumpp parameters
	cl.repeats <- 1000
	cl.s <- FALSE
	cl.m <- 'LargeKGreedy'
	# BayeScan parameters
	bs.reps <- 4
	bs.fdr <- 0.1
	bs.threads <- 10
	bs.n <- 5000
	bs.thin <- 10
	bs.nbp <- 20
	bs.pilot <- 50000
	bs.burn <- 50000
	# MDS parameters
	mds.k <- 2
	mds.trymax <- 100
	# Gurobi parameters
	gb.Threads <- 10
	gb.MIPGap <- 0.05
	# targets
	rapr.amount.target <- 0.2
	rapr.surrogate.target <- 0.9
	rapr.genetic.target <- 0.9
	rapr.pareto.surrogate.targets <- seq(0.01, 1, length.out=3)
}

# save workspace
save.session('results/.cache/00-initialization.rda')

