## load .rda
session::restore.session('results/.cache/06-format-data-for-prioritisations.rda')

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
					geo.surrogate.targets=NA,
					adaptive.genetic.targets=NA,
					neutral.genetic.targets=NA,
					Threads=general.params.LST[[MODE]]$threads,
					MIPGap=gurobi.params.LST[[MODE]]$MIPGap,
					NumberSolutions=rapr.params.LST[[MODE]]$surrogacy.analysis$surrogate.optimal.replicates
				)
			}
		)
	},
	.progress='text',
	.parallel=FALSE
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
					env.surrogate.targets=NA,
					geo.surrogate.targets=j,
					adaptive.genetic.targets=NA,
					neutral.genetic.targets=NA,
					Threads=general.params.LST[[MODE]]$threads,
					MIPGap=gurobi.params.LST[[MODE]]$MIPGap,
					NumberSolutions=rapr.params.LST[[MODE]]$surrogacy.analysis$surrogate.optimal.replicates
				)
			}
		)
	},
	.progress='text',
	.parallel=FALSE
)

# extract results
env.correlation.DF <- ldply(
	seq_along(env.correlation.prioritisations), 
	function(i) {
		mutate(
			ldply(env.correlation.prioritisations[[i]], extractResults),
			Surrogate.target=rep(
				rapr.params.LST[[MODE]]$surrogacy.analysis$surrogate.target,
				each=rapr.params.LST[[MODE]]$surrogacy.analysis$surrogate.optimal.replicates
			),
			genetic.held=adaptive.held,
			Type='Environmental'
		)
	}
) %>% mutate(
	genetic.held=replace(genetic.held, which(genetic.held<0), 0),
	adaptive.held=replace(adaptive.held, which(adaptive.held<0), 0),
	neutral.held=replace(neutral.held, which(neutral.held<0),0)
)

geo.correlation.DF <- ldply(
	seq_along(geo.correlation.prioritisations),
	function(i) {
		mutate(
			ldply(geo.correlation.prioritisations[[i]], extractResults),
			Surrogate.target=rep(
				rapr.params.LST[[MODE]]$surrogacy.analysis$surrogate.target,
				each=rapr.params.LST[[MODE]]$surrogacy.analysis$surrogate.optimal.replicates
			),
			genetic.held=neutral.held,
			Type='Geographic'
		)
	}
) %>% mutate(
	genetic.held=replace(genetic.held, which(genetic.held<0), 0),
	adaptive.held=replace(adaptive.held, which(adaptive.held<0), 0),
	neutral.held=replace(neutral.held, which(neutral.held<0),0)
)

correlation.DF <- rbind(env.correlation.DF, geo.correlation.DF)

## save .rda
save.session('results/.cache/07-surrogacy-prioritizations.rda', compress='xz')

