## load .rda
session::restore.session('results/.cache/05-format-data-for-prioritisations.rda')

## load parameters
rapr.params.LST <- parseTOML('parameters/rapr.toml')
gurobi.params.LST <- parseTOML('parameters/gurobi.toml')

## correlation analysis
# generate prioritistions
cat("starting environmental surrogacy prioritizations\n")
env.correlation.prioritisations <- llply(
	seq_along(unique(spp.samples.DF$species)),
	function(i) {
		llply(
			rapr.params.LST[[MODE]]$surrogacy.analysis$surrogate.target,
			function(j) {
				species.prioritisation(
					x=spp.subset(ru, i),
					amount.targets=0,
					env.surrogate.targets=j,
					geo.surrogate.targets=0,
					adaptive.genetic.targets=0,
					neutral.genetic.targets=0,
					Threads=gurobi.params.LST[[MODE]]$Threads,
					MIPGap=gurobi.params.LST[[MODE]]$MIPGap
				)
			}
		)
	},
	.progress='text'
)

cat("starting geographic surrogacy prioritizations\n")
geo.correlation.prioritisations <- llply(
	seq_along(unique(spp.samples.DF$species)),
	function(i) {
		llply(
			rapr.params.LST[[MODE]]$surrogacy.analysis$surrogate.target,
			function(j) {
				species.prioritisation(
					x=spp.subset(ru, i),
					amount.targets=0,
					env.surrogate.targets=0,
					geo.surrogate.targets=j,
					adaptive.genetic.targets=0,
					neutral.genetic.targets=0,
					Threads=gurobi.params.LST[[MODE]]$Threads,
					MIPGap=gurobi.params.LST[[MODE]]$MIPGap
				)
			}
		)
	},
	.progress='text'
)

# extract results
env.correlation.DF <- ldply(
	seq_along(env.correlation.prioritisations), 
	function(i) {
		mutate(
			ldply(env.correlation.prioritisations[[i]], extractResults),
			Surrogate.target=rapr.params.LST[[MODE]]$surrogacy.analysis$surrogate.target
		)
	}
)

geo.correlation.DF <- ldply(
	seq_along(geo.correlation.prioritisations), 
	function(i) {
		mutate(
			ldply(geo.correlation.prioritisations[[i]], extractResults),
			Surrogate.target=rapr.params.LST[[MODE]]$surrogacy.analysis$surrogate.target
		)
	}
)

## save .rda
save.session('results/.cache/06-surrogacy-prioritizations.rda')

