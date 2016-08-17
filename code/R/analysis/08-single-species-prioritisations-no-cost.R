## load .rda
checkpoint::checkpoint(general.params.LST[[MODE]]$checkpoint_date, R.version=general.params.LST[[MODE]]$checkpoint_R_version, scanForPackages=FALSE)
session::restore.session('data/intermediate/06-format-data-for-prioritisations.rda')

## load parameters
raptr.params.LST <- parseTOML('code/parameters/raptr.toml')
gurobi.params.LST <- parseTOML('code/parameters/gurobi.toml')

#### single species analysis
### generate RapSolved objects
## amount-based prioritisations
# prepare cluster object
clust <- makeCluster(general.params.LST[[MODE]]$threads, type='PSOCK', outfile="")
clusterEvalQ(clust, {library(raptr)})
clusterExport(clust, c('spp.samples.sub.DF','grid.sub.DF','raptr.params.LST',
	'MODE', 'species.prioritisation', 'ru'))
registerDoParallel(clust)
# main analysis
single.spp.amount.prioritisations <- llply(
	seq_along(unique(spp.samples.sub.DF$species)),
	function(x) {
		# identify which planning units are occupied by the species
		curr.pu <- which(grid.sub.DF[[unique(spp.samples.sub.DF$species)[[x]]]]==1)
		n.pu <- ceiling(length(curr.pu) * raptr.params.LST[[MODE]]$scenario.analysis$amount.target)
		zeros <- rep(0, length=nrow(grid.sub.DF))
		curr.sol.MTX<-t(replicate(
			n=raptr.params.LST[[MODE]]$scenario.analysis$single.species.amount.replicates,
			expr={replace(zeros, sample(x=curr.pu, size=n.pu), rep(1, n.pu))}
		))
		# generate portfolio of random selections
		return(
			species.prioritisation(
				x=spp.subset(ru, x),
				amount.targets=raptr.params.LST[[MODE]]$scenario.analysis$amount.target,
				env.surrogate.targets=NA,
				geo.surrogate.targets=NA,
				adaptive.genetic.targets=NA,
				neutral.genetic.targets=NA,
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
	seq_along(unique(spp.samples.sub.DF$species)),
	function(x) {
		species.prioritisation(
				x=spp.subset(ru, x),
				amount.targets=raptr.params.LST[[MODE]]$scenario.analysis$amount.target,
				env.surrogate.targets=raptr.params.LST[[MODE]]$scenario.analysis$surrogate.target,
				geo.surrogate.targets=raptr.params.LST[[MODE]]$scenario.analysis$surrogate.target,
				adaptive.genetic.targets=NA,
				neutral.genetic.targets=NA,
				Threads=general.params.LST[[MODE]]$threads,
				MIPGap=gurobi.params.LST[[MODE]]$MIPGap,
				NumberSolutions=raptr.params.LST[[MODE]]$scenario.analysis$other.replicates,
				MultipleSolutionsMethod='benders.cuts'
		)
	}
)

## genetic-based prioritisations
single.spp.genetic.prioritisations <- llply(
	seq_along(unique(spp.samples.sub.DF$species)),
	function(x) {
		species.prioritisation(
				x=spp.subset(ru, x),
				amount.targets=raptr.params.LST[[MODE]]$scenario.analysis$amount.target,
				env.surrogate.targets=NA,
				geo.surrogate.targets=NA,
				adaptive.genetic.targets=raptr.params.LST[[MODE]]$scenario.analysis$genetic.target,
				neutral.genetic.targets=raptr.params.LST[[MODE]]$scenario.analysis$genetic.target,
				Threads=general.params.LST[[MODE]]$threads,
				MIPGap=gurobi.params.LST[[MODE]]$MIPGap,
				NumberSolutions=raptr.params.LST[[MODE]]$scenario.analysis$other.replicates
		)
	}
)

## post-processing
# combine lists of seperate prioritisations
single.spp.prioritisations <- llply(
	seq_along(unique(spp.samples.sub.DF$species)),
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
				rep('Amount',raptr.params.LST[[MODE]]$scenario.analysis$single.species.amount.replicates),
				rep('Surrogate',raptr.params.LST[[MODE]]$scenario.analysis$other.replicates),
				rep('Genetic',raptr.params.LST[[MODE]]$scenario.analysis$other.replicates)
			)
		)
	}
)  %>% mutate(
	adaptive.held=replace(adaptive.held, which(adaptive.held<0), 0),
	neutral.held=replace(neutral.held, which(neutral.held<0),0)
)

## save .rda
save.session('data/intermediate/08-single-species-prioritisations-no-cost.rda', compress='xz')
