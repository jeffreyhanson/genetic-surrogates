## load .rda
session::restore.session('results/.cache/07-multi-species-prioritisations-no-cost.rda')

## multispecies analysis
# make prioritisations
multi.spp.prioritisations.with.cost <- llply(
	list(
		list(rapr.params.LST[[MODE]]$multi.species$amount.target,0,0,
			rapr.params.LST[[MODE]]$multi.species$amount.replicates),
		list(rapr.params.LST[[MODE]]$multi.species$amount.target,
			rapr.params.LST[[MODE]]$multi.species$surrogate.target,0,
			rapr.params.LST[[MODE]]$multi.species$surrogate.replicates), 
		list(rapr.params.LST[[MODE]]$multi.species$amount.target,0,
			rapr.params.LST[[MODE]]$multi.species$genetic.target,
			rapr.params.LST[[MODE]]$multi.species$genetic.replicates)
	), 
	function(y) {
		species.prioritisation(
			x=ru_with_cost,
			amount.targets=y[[1]],
			env.surrogate.targets=y[[2]],
			geo.surrogate.targets=y[[2]],
			adaptive.genetic.targets=y[[3]],
			neutral.genetic.targets=y[[3]],
			Threads=gurobi.params.LST[[MODE]]$Threads,
			MIPGap=gurobi.params.LST[[MODE]]$MIPGap,
			NumberSolutions=y[[4]]
		)
	}
)

# generate results table
multi.spp.with.cost.DF <- ldply(
	seq_along(multi.spp.prioritisations.with.cost),
	.fun=function(i) {
		mutate(
			extractResults(multi.spp.prioritisations.with.cost[[i]]),
			Prioritisation=c('Amount','Surrogate','Genetic')[i]
		)
	}
)

## save .rda
save.session('results/.cache/08-multi-species-prioritisations-with-cost.rda')
