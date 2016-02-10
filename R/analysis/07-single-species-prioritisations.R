## load .rda
session::restore.session('results/.cache/05-format-data-for-prioritisations.rda')

## load parameters
rapr.params.LST <- parseTOML('parameters/rapr.toml')
gurobi.params.LST <- parseTOML('parameters/gurobi.toml')

#### single species analysis
### generate RapSolved objects
## amount-based prioritisations
# prepare cluster object
clust <- makeCluster(gurobi.params.LST[[MODE]]$Threads, type='SOCK')
clusterEvalQ(clust, {library(rapr)})
clusterExport(clust, c('spp.samples.DF','grid.DF','rapr.params.LST',
	'MODE', 'species.prioritisation', 'ru'))
registerDoParallel(clust)
# main analysis
single.spp.amount.prioritisations <- llply(
	seq_along(unique(spp.samples.DF$species)),
	function(x) {
		# identify which planning units are occupied by the species
		curr.pu <- which(grid.DF[[unique(spp.samples.DF$species)[[x]]]]==1)
		n.pu <- ceiling(length(curr.pu) * rapr.params.LST[[MODE]]$single.species$amount.target)
		zeros <- rep(0, length=nrow(grid.DF))
		curr.sol.MTX<-t(replicate(
			n=rapr.params.LST[[MODE]]$single.species$amount.replicates,
			expr={replace(zeros, sample(x=curr.pu, size=n.pu), rep(1, n.pu))}
		))
		# generate portfolio of random selections
		return(
			species.prioritisation(
				x=spp.subset(ru, x),
				amount.targets=rapr.params.LST[[MODE]]$single.species$amount.target,
				env.surrogate.targets=0,
				geo.surrogate.targets=0,
				adaptive.genetic.targets=0,
				neutral.genetic.targets=0,
				b=curr.sol.MTX
			)
		)
	},
	.parallel=TRUE
)
# kill cluster
clust <- stopCluster(clust)

## surrogate-based priortisations
single.spp.surrogate.prioritisations <- llply(
	seq_along(unique(spp.samples.DF$species)),
	function(x) {
		species.prioritisation(
				x=spp.subset(ru, x),
				amount.targets=rapr.params.LST[[MODE]]$single.species$amount.target,
				env.surrogate.targets=rapr.params.LST[[MODE]]$single.species$surrogate.target,
				geo.surrogate.targets=rapr.params.LST[[MODE]]$single.species$surrogate.target,
				adaptive.genetic.targets=0,
				neutral.genetic.targets=0,
				Threads=gurobi.params.LST[[MODE]]$Threads,
				MIPGap=gurobi.params.LST[[MODE]]$MIPGap,
				NumberSolutions=rapr.params.LST[[MODE]]$single.species$surrogate.replicates
		)
	}
)

## genetic-based prioritisations
single.spp.genetic.prioritisations <- llply(
	seq_along(unique(spp.samples.DF$species)),
	function(x) {
		species.prioritisation(
				x=spp.subset(ru, x),
				amount.targets=rapr.params.LST[[MODE]]$single.species$amount.target,
				env.surrogate.targets=0,
				geo.surrogate.targets=0,
				adaptive.genetic.targets=rapr.params.LST[[MODE]]$single.species$genetic.target,
				neutral.genetic.targets=rapr.params.LST[[MODE]]$single.species$genetic.target,
				Threads=gurobi.params.LST[[MODE]]$Threads,
				MIPGap=gurobi.params.LST[[MODE]]$MIPGap,
				NumberSolutions=rapr.params.LST[[MODE]]$single.species$genetic.replicates
		)
	}
)

## post-processing
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
			ldply(x, extractResults),
			Prioritisation=c(
				rep('Amount',rapr.params.LST[[MODE]]$single.species$amount.replicates),
				rep('Surrogate',rapr.params.LST[[MODE]]$single.species$surrogate.replicates),
				rep('Genetic',rapr.params.LST[[MODE]]$single.species$genetic.replicates)
			)
		)
	}
)

## save .rda
save.session('results/.cache/07-single-species-prioritisations.rda')
