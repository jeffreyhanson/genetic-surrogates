## multispecies analysis
# make prioritisations
multi.spp.prioritisations <- llply(
	list(
		c(rapr.amount.target,0,0), c(rapr.amount.target,rapr.surrogate.target,0), 
			c(rapr.amount.target,0,rapr.genetic.target,0)
	), 
	function(y) {
		species.prioritisation(
			x=ru,
			amount.targets=y[1],
			env.surrogate.targets=y[2],
			geo.surrogate.targets=y[2],
			adaptive.genetic.targets=y[3],
			neutral.genetic.targets=y[3],
			Threads=gb.Threads,
			MIPGap=gb.MIPGap
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
 