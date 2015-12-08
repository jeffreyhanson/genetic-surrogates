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

