## load .rda
checkpoint::checkpoint(general.params.LST[[MODE]]$checkpoint_date, R.version=general.params.LST[[MODE]]$checkpoint_R_version, scanForPackages=FALSE)
session::restore.session('data/intermediate/06-format-data-for-prioritisations.rda')

## load parameters
raptr.params.LST <- parseTOML('code/parameters/raptr.toml')
gurobi.params.LST <- parseTOML('code/parameters/gurobi.toml')

## multispecies analysis
# make prioritisations
multi.spp.prioritisations <- llply(
	list(
		list(raptr.params.LST[[MODE]]$scenario.analysis$amount.target,NA,NA,
			raptr.params.LST[[MODE]]$scenario.analysis$other.replicates),
		list(raptr.params.LST[[MODE]]$scenario.analysis$amount.target,
			raptr.params.LST[[MODE]]$scenario.analysis$surrogate.target,NA,
			raptr.params.LST[[MODE]]$scenario.analysis$other.replicates), 
		list(raptr.params.LST[[MODE]]$scenario.analysis$amount.target,NA,
			raptr.params.LST[[MODE]]$scenario.analysis$genetic.target,
			raptr.params.LST[[MODE]]$scenario.analysis$other.replicates)
	), 
	function(y) {
		species.prioritisation(
			x=ru,
			amount.targets=y[[1]],
			env.surrogate.targets=y[[2]],
			geo.surrogate.targets=y[[2]],
			adaptive.genetic.targets=y[[3]],
			neutral.genetic.targets=y[[3]],
			Threads=general.params.LST[[MODE]]$threads,
			MIPGap=gurobi.params.LST[[MODE]]$MIPGap,
			NumberSolutions=y[[4]]
		)
	}
)

# generate results table
multi.spp.DF <- ldply(
	seq_along(multi.spp.prioritisations),
	.fun=function(i) {
		mutate(
			extractResults(multi.spp.prioritisations[[i]]),
			Prioritisation=c('Amount','Surrogate','Genetic')[i]
		)
	}
)  %>% mutate(
	adaptive.held=replace(adaptive.held, which(adaptive.held<0), 0),
	neutral.held=replace(neutral.held, which(neutral.held<0),0)
)


## save .rda
save.session('data/intermediate/10-multi-species-prioritisations-no-cost.rda', compress='xz')
