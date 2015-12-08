## ---- include=FALSE------------------------------------------------------
# set global cache
knitr::opts_chunk$set(cache=TRUE)

# load packages
library(genetic.surrogates)
library(data.table)
library(rgeos)
library(ggplot2)
library(grid)
library(gridExtra)
library(plotrix)
library(plyr)
library(dplyr)
library(tidyr)
library(pander)
library(multcomp)

# set pander options
panderOptions('knitr.auto.asis', FALSE)

# set default select method
select <- dplyr::select

### set parameters
## debugging parameters
# number of species
n.spp <- 3
# BayeScan parameters
bs.threshold <- 0.5
bs.threads <- 1
bs.n <- 50
bs.thin <- 1
bs.nbp <- 10
bs.pilot <- 50
bs.burn <- 10
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

## analysis parameters
## number of species
## n.spp <- 27
## BayeScan parameters
# bs.threshold <- 0.95
# bs.threads <- 10
# bs.n <- 5000
# bs.thin <- 10
# bs.nbp <- 20
# bs.pilot <- 50000
# bs.burn <- 50000
# MDS parameters
# mds.k <- 2
# mds.trymax <- 100
## Gurobi parameters
# gb.Threads <- 10
# gb.MIPGap <- 0.05

# set seed for reproducibility
set.seed(500)

## ------------------------------------------------------------------------
knitr::kable(
	format.table(
		ldply(
			seq_along(unique(spp.samples.DF$species)), 
			function(i) {
				data.frame(
					Species=paste0('\\textit{',gsub('\\_', ' ', unique(spp.samples.DF$species)[i]),'}'),
					Primer=spp.BayeScanData.LST[[i]]@primers,
					Probability=spp.BayeScan.LST[[i]]@results@fst[[2]],
					qval=spp.BayeScan.LST[[i]]@results@fst[[4]],
					alpha=spp.BayeScan.LST[[i]]@results@fst[[5]],
					fst=spp.BayeScan.LST[[i]]@results@fst[[6]],
					Type=spp.BayeScan.LST[[i]]@results@fst[[7]]
				)
			}
		),
		omit='Type'
	),
	digits=2,
	colnames=c('Species', 'Primer', 'Probability', 'q-value', '$\\alpha$', '$F_{ST}$', 'Type'),
	align=c('l', 'c', 'c', 'c', 'c', 'c', 'c')
)

## ------------------------------------------------------------------------
knitr::kable(
	format.table(
		ldply(
			seq_along(unique(spp.samples.DF$species)), 
			function(i) {
				ldply(
					seq_along(spp.mds.LST[[i]]),
					function(j) {
					data.frame(
						Species=paste0('\\textit{',gsub('\\_', ' ', unique(spp.samples.DF$species)[i]),'}'),
						Loci=names(spp.mds.LST[[i]])[j],
						Stress=spp.mds.LST[[i]][[j]]$stress,
						Converged=spp.mds.LST[[i]][[j]]$converged
					)
				})
			}
		),
		omit='Converged'
	),
	digits=2,
	caption='Summary of non-metric multi-dimensional scaling (MDS) analyses on genetic variation for each species.',
	colnames=c('Species', 'Loci Type', 'NMDS Stress', 'Converged'),
	align=c('l', 'c', 'c', 'c')
)

