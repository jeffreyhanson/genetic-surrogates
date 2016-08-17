## load .rda
checkpoint::checkpoint('2016-08-03', R.version='3.3.0', scanForPackages=FALSE)
session::restore.session('data/intermediate/06-format-data-for-prioritisations.rda')

## load parameters
raptr.params.LST <- parseTOML('code/parameters/raptr.toml')
gurobi.params.LST <- parseTOML('code/parameters/gurobi.toml')

#### single species analysis
### generate RapSolved objects
## amount-based prioritisations
# main analysis
single.spp.amount.prioritisations.with.cost <- llply(
	seq_along(unique(spp.samples.sub.DF$species)),
	function(x) {
		# generate portfolio of random selections
		return(
			species.prioritisation(
				x=spp.subset(ru_with_cost, x),
				amount.targets=raptr.params.LST[[MODE]]$scenario.analysis$amount.target,
				env.surrogate.targets=NA,
				geo.surrogate.targets=NA,
				adaptive.genetic.targets=NA,
				neutral.genetic.targets=NA,
				Threads=general.params.LST[[MODE]]$threads,
				MIPGap=gurobi.params.LST[[MODE]]$MIPGap,
				NumberSolutions=raptr.params.LST[[MODE]]$scenario.analysis$other.replicates,
				MultipleSolutionsMethod='benders.cuts'
			)
		)
	}
)

## surrogate-based priortisations
single.spp.surrogate.prioritisations.with.cost <- llply(
	seq_along(unique(spp.samples.sub.DF$species)),
	function(x) {
		species.prioritisation(
				x=spp.subset(ru_with_cost, x),
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
single.spp.genetic.prioritisations.with.cost <- llply(
	seq_along(unique(spp.samples.sub.DF$species)),
	function(x) {
		species.prioritisation(
				x=spp.subset(ru_with_cost, x),
				amount.targets=raptr.params.LST[[MODE]]$scenario.analysis$amount.target,
				env.surrogate.targets=NA,
				geo.surrogate.targets=NA,
				adaptive.genetic.targets=raptr.params.LST[[MODE]]$scenario.analysis$genetic.target,
				neutral.genetic.targets=raptr.params.LST[[MODE]]$scenario.analysis$genetic.target,
				Threads=general.params.LST[[MODE]]$threads,
				MIPGap=gurobi.params.LST[[MODE]]$MIPGap,
				NumberSolutions=raptr.params.LST[[MODE]]$scenario.analysis$other.replicates,
				MultipleSolutionsMethod='benders.cuts'
		)
	}
)

## post-processing
# combine lists of seperate prioritisations
single.spp.prioritisations.with.cost <- llply(
	seq_along(unique(spp.samples.sub.DF$species)),
	function(i) {
		list(
			single.spp.amount.prioritisations.with.cost[[i]],
			single.spp.surrogate.prioritisations.with.cost[[i]],
			single.spp.genetic.prioritisations.with.cost[[i]]
		)
	}
)

# generate results table
single.spp.with.cost.DF <- ldply(
	single.spp.prioritisations.with.cost,
	function(x) {
		mutate(
			ldply(x, extractResults),
			Prioritisation=c(
				rep('Amount',raptr.params.LST[[MODE]]$scenario.analysis$other.replicates),
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
save.session('data/intermediate/09-single-species-prioritisations-with-cost.rda', compress='xz')
