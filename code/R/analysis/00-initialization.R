#### set default error message
options(error=function() {
  save.image(paste0('results-error-',as.character(Sys.time()),'.rda'))
  traceback()
})

# parse args
args <- commandArgs(TRUE)
if (length(args)>0) {
	print(args)
	if (grepl('MODE',args))
		MODE <- strsplit(grep('MODE', args, value=TRUE), '=', fixed=TRUE)[[1]][[2]]
}
	
	
#### Load pacakges
## load bioconductor packages
# install packages
# source('http://bioconductor.org/biocLite.R')
# biocLite('qvalue')

# load CRAN packages
suppressMessages(library(stats))
suppressMessages(library(data.table))
suppressMessages(library(grid))
suppressMessages(library(gridExtra))
suppressMessages(library(plotrix))
suppressMessages(library(plyr))
suppressMessages(library(dplyr))
suppressMessages(library(tidyr))
suppressMessages(library(pander))
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
suppressMessages(library(RcppTOML))
suppressMessages(library(optimx))
suppressMessages(library(RColorBrewer))
suppressMessages(library(mclust))
suppressMessages(library(cluster))
suppressMessages(library(RVAideMemoire))
suppressMessages(library(pcadapt))

## load github packages
# devtools::install_github('paleo13/raspr')
suppressMessages(library(rapr))
# devtools::install_github('paleo13/bayescanr')
suppressMessages(library(bayescanr))
# devtools::install_github('paleo13/structurer')
suppressMessages(library(structurer))
# devtools::install_github('paleo13/ggplot2')
suppressMessages(library(ggplot2))
# devtools::install_github('paleo13/rgurobi')
suppressMessages(library(rgurobi))

# set pander options
panderOptions('knitr.auto.asis', FALSE)

# set seed for reproducibility
set.seed(500)

# set default select method
select <- dplyr::select

### Load functions
for (x in dir(file.path('code', 'R', 'functions'), full.names=TRUE)) source(x)

### set parameters
if (!exists('MODE')) MODE <- 'debug'
cat('MODE = ',MODE,'\n')
general.params.LST <- parseTOML('code/parameters/general.toml')

# save workspace
save.session('data/intermediate/00-initialization.rda', compress='xz')

