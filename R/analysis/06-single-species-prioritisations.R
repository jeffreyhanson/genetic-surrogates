## load .rda
session::restore.session('results/.cache/05-format-data-for-prioritisations.rda')

## load parameters
rapr.params.LST <- parseTOML('parameters/rapr.toml')
gurobi.params.LST <- parseTOML('parameters/gurobi.toml')

### single species analysis
## generate RapSolved objects
# amount-based prioritisations
single.spp.amount.prioritisations <- llply(
	seq_along(unique(spp.samples.DF$species)),
	function(x) {
		# identify which planning units are occupied by the species
		curr.pu <- which(grid.DF[[unique(spp.samples.DF$species)[[x]]]]==1)
		n.pu <- ceiling(length(curr.pu) * rapr.params.LST[[MODE]]$amount.target)
		# generate portfolio of random selections
		return(
			llply(
				seq_len(rapr.params.LST[[MODE]]$single.species.amount.replicates),
				function(r) {
					species.prioritisation(
						x=spp.subset(ru, x),
						amount.targets=rapr.params.LST[[MODE]]$amount.target,
						env.surrogate.targets=rapr.params.LST[[MODE]]$surrogate.target,
						geo.surrogate.targets=rapr.params.LST[[MODE]]$surrogate.target,
						adaptive.genetic.targets=0,
						neutral.genetic.targets=0,
						b=sample(curr.pu, n.pu)
					)
				}
			)
		)
	}
)

# surrogate-based priortisations
single.spp.surrogate.prioritisations <- llply(
	seq_along(unique(spp.samples.DF$species)),
	function(x) {
		species.prioritisation(
				x=spp.subset(ru, x),
				amount.targets=rapr.params.LST[[MODE]]$amount.target,
				env.surrogate.targets=rapr.params.LST[[MODE]]$surrogate.target,
				geo.surrogate.targets=rapr.params.LST[[MODE]]$surrogate.target,
				adaptive.genetic.targets=0,
				neutral.genetic.targets=0,
				Threads=gurobi.params.LST[[MODE]]$Threads,
				MIPGap=gurobi.params.LST[[MODE]]$MIPGap
		)
	}
)

# genetic-based prioritisations
single.spp.genetic.prioritisations <- llply(
	seq_along(unique(spp.samples.DF$species)),
	function(x) {
		species.prioritisation(
				x=spp.subset(ru, x),
				amount.targets=rapr.params.LST[[MODE]]$amount.target,
				env.surrogate.targets=0,
				geo.surrogate.targets=0,
				adaptive.genetic.targets=rapr.params.LST[[MODE]]$genetic.target,
				neutral.genetic.targets=rapr.params.LST[[MODE]]$genetic.target,
				Threads=gurobi.params.LST[[MODE]]$Threads,
				MIPGap=gurobi.params.LST[[MODE]]$MIPGap
		)
	}
)

# combine lists of seperate prioritisations
single.spp.prioritisations <- llply(
	seq_along(unique(spp.samples.DF$species)),
	function(i) {
		list(
			single.spp.amount.prioritisations[[i]],
			single.spp.surrogate.prioritisations[[i]],
			single.spp.genetic.prioritisations[[i]]
		)
	}
)

# generate results table
single.spp.DF <- ldply(
	single.spp.prioritisations,
	function(x) {
		mutate(
			ldply(x, function(y) {
				if (!inherits(y, 'list'))
					y <- list(y)
				ldply(y, extractResults)
			}),
			Prioritisation=c(
				rep('Amount',rapr.params.LST[[MODE]]$single.species.amount.replicates),
				'Surrogate', 'Genetic')
		)
	}
)

# remove all but the first amount-based prioritisation for each species to reduce disk usage
single.spp.prioritisations <- llply(
	seq_along(unique(spp.samples.DF$species)),
	function(i) {
		list(
			single.spp.amount.prioritisations[[i]][[1]],
			single.spp.surrogate.prioritisations[[i]],
			single.spp.genetic.prioritisations[[i]]
		)
	}
)

# remove objects to clear disk usage
rm(single.spp.amount.prioritisations)
rm(single.spp.surrogate.prioritisations)
rm(single.spp.genetic.prioritisations)

## save .rda
save.session('results/.cache/06-single-species-prioritisations.rda')
