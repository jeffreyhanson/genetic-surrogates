## load .rda
session::restore.session('results/.cache/05-format-data-for-prioritisations.rda')

## load parameters
rapr.params.LST <- parseTOML('parameters/rapr.toml')
gurobi.params.LST <- parseTOML('parameters/gurobi.toml')

## multispecies analysis
# make prioritisations
multi.spp.prioritisations <- llply(
	list(
		list(rapr.params.LST[[MODE]]$multi.species$amount.target,NA,NA,
			rapr.params.LST[[MODE]]$multi.species$amount.replicates),
		list(rapr.params.LST[[MODE]]$multi.species$amount.target,
			rapr.params.LST[[MODE]]$multi.species$surrogate.target,NA,
			rapr.params.LST[[MODE]]$multi.species$surrogate.replicates), 
		list(rapr.params.LST[[MODE]]$multi.species$amount.target,NA,
			rapr.params.LST[[MODE]]$multi.species$genetic.target,
			rapr.params.LST[[MODE]]$multi.species$genetic.replicates)
	), 
	function(y) {
		species.prioritisation(
			x=ru,
			amount.targets=y[[1]],
			env.surrogate.targets=y[[2]],
			geo.surrogate.targets=y[[2]],
			adaptive.genetic.targets=y[[3]],
			neutral.genetic.targets=y[[3]],
			Threads=gurobi.params.LST[[MODE]]$Threads,
			MIPGap=gurobi.params.LST[[MODE]]$MIPGap,
			NumberSolutions=y[[4]],
			MultipleSolutionsMethod='solution.pool'
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
save.session('results/.cache/08-multi-species-prioritisations-no-cost.rda')
