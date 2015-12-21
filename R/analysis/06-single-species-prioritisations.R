## load .rda
session::restore.session('results/.cache/05-format-data-for-prioritisations.rda')

## single species analysis
# generate RapSolved objects
single.spp.prioritisations <- llply(
	seq_along(unique(spp.samples.DF$species)),
	function(x) {
		llply(
			list(
				c(rapr.amount.target,0,0), c(rapr.amount.target,rapr.surrogate.target,0), 
					c(rapr.amount.target,0,rapr.genetic.target,0)
			), 
			function(y) {
				species.prioritisation(
					x=spp.subset(ru, x),
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
	}
)

# generate results table
single.spp.DF <- ldply(
	single.spp.prioritisations,
	function(x) {
		mutate(
			ldply(x, extractResults),
			Prioritisation=c('Amount', 'Surrogate', 'Genetic')
		)
	}
)

## save .rda
save.session('results/.cache/06-single-species-prioritisations.rda')
