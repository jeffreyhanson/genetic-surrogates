## load .rda
session::restore.session('data/results/06-format-data-for-prioritisations.rda')

## load parameters
rapr.params.LST <- parseTOML('code/parameters/rapr.toml')
gurobi.params.LST <- parseTOML('code/parameters/gurobi.toml')

## multispecies analysis
# make prioritisations
multi.spp.prioritisations <- llply(
	list(
		list(rapr.params.LST[[MODE]]$scenario.analysis$amount.target,NA,NA,
			rapr.params.LST[[MODE]]$scenario.analysis$other.replicates),
		list(rapr.params.LST[[MODE]]$scenario.analysis$amount.target,
			rapr.params.LST[[MODE]]$scenario.analysis$surrogate.target,NA,
			rapr.params.LST[[MODE]]$scenario.analysis$other.replicates), 
		list(rapr.params.LST[[MODE]]$scenario.analysis$amount.target,NA,
			rapr.params.LST[[MODE]]$scenario.analysis$genetic.target,
			rapr.params.LST[[MODE]]$scenario.analysis$other.replicates)
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
save.session('data/results/09-multi-species-prioritisations-no-cost.rda', compress='xz')
