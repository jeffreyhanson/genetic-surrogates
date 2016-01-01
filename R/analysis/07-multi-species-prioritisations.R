## load .rda
session::restore.session('results/.cache/06-single-species-prioritisations.rda')

## multispecies analysis
# make prioritisations
multi.spp.prioritisations <- llply(
	list(
		c(rapr.params.LST[[MODE]]$amount.target,0,0), c(rapr.params.LST[[MODE]]$amount.target,rapr.params.LST[[MODE]]$surrogate.target,0), 
			c(rapr.params.LST[[MODE]]$amount.target,0,rapr.params.LST[[MODE]]$genetic.target,0)
	), 
	function(y) {
		species.prioritisation(
			x=ru,
			amount.targets=y[1],
			env.surrogate.targets=y[2],
			geo.surrogate.targets=y[2],
			adaptive.genetic.targets=y[3],
			neutral.genetic.targets=y[3],
			Threads=gurobi.params.LST[[MODE]]$Threads,
			MIPGap=gurobi.params.LST[[MODE]]$MIPGap
		)
	}
)

# generate results table
multi.spp.DF <- ldply(seq_along(multi.spp.prioritisations), function(i) {
	mutate(
		extractResults(multi.spp.prioritisations[[i]]),
		Prioritisation=c('Amount', 'Surrogate', 'Genetic')[i]
	)
})

## save .rda
save.session('results/.cache/07-multi-species-prioritisations.rda')
