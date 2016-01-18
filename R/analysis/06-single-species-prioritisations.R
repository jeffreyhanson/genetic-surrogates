## load .rda
session::restore.session('results/.cache/05-format-data-for-prioritisations.rda')

## load parameters
rapr.params.LST <- parseTOML('parameters/rapr.toml')
gurobi.params.LST <- parseTOML('parameters/gurobi.toml')

## single species analysis
# generate RapSolved objects
single.spp.prioritisations <- llply(
	seq_along(unique(spp.samples.DF$species)),
	function(x) {
		llply(
			list(
				c(rapr.params.LST[[MODE]]$amount.target,0,0),
				c(rapr.params.LST[[MODE]]$amount.target,rapr.params.LST[[MODE]]$surrogate.target,0), 
				c(rapr.params.LST[[MODE]]$amount.target,0,rapr.params.LST[[MODE]]$genetic.target)
			), 
			function(y) {
				species.prioritisation(
					x=spp.subset(ru, x),
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
