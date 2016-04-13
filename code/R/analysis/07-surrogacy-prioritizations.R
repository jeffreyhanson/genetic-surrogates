## load .rda
session::restore.session('data/results/06-format-data-for-prioritisations.rda')

## load parameters
rapr.params.LST <- parseTOML('code/parameters/rapr.toml')
gurobi.params.LST <- parseTOML('code/parameters/gurobi.toml')

## correlation analysis
# generate prioritistions
cat("starting environmental surrogacy prioritizations\n")
env.correlation.prioritisations <- llply(
	seq_along(unique(spp.samples.sub.DF$species)),
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
					NumberSolutions=rapr.params.LST[[MODE]]$surrogacy.analysis$optimal.replicates
				)
			}
		)
	},
	.progress='text',
	.parallel=FALSE
)

cat("starting random prioritizations for the environmental surrogate prioritizations \n")
clust <- makeCluster(general.params.LST[[MODE]]$threads, type='SOCK')
clusterEvalQ(clust, {library(rapr)})
clusterExport(clust, c('spp.samples.sub.DF','grid.sub.DF','rapr.params.LST',
	'MODE', 'species.prioritisation', 'ru', 'env.correlation.prioritisations'))
registerDoParallel(clust)
env.random.prioritisations <- llply(
	seq_along(unique(spp.samples.sub.DF$species)),
	function(i) {
		llply(
			seq_along(rapr.params.LST[[MODE]]$surrogacy.analysis$surrogate.target),
			function(j) {
				setMKLthreads(1)
				# extract number of planning units
				curr.optim_n <- table(rowSums(env.correlation.prioritisations[[i]][[j]]@results@selections))
				curr.optim_p <- curr.optim_n / sum(curr.optim_n)
				# identify which planning units are occupied by the species
				curr.pu <- which(grid.sub.DF[[unique(spp.samples.sub.DF$species)[[i]]]]==1)
				zeros <- rep(0, length=nrow(grid.sub.DF))
				# generate portfolio of random selections
				curr.random_n <- as.numeric(sample(
					names(curr.optim_n),
					size=rapr.params.LST[[MODE]]$surrogacy.analysis$random.replicates,
					replace=TRUE, prob=as.numeric(curr.optim_p)
				))
				curr.sol.MTX<-t(sapply(
					curr.random_n,
					function(n) {replace(zeros, sample(x=curr.pu, size=n), rep(1, n))}
				))
				# generate RapSolved object
				species.prioritisation(
					x=spp.subset(ru, i),
					amount.targets=0.0001,
					env.surrogate.targets=NA,
					geo.surrogate.targets=NA,
					adaptive.genetic.targets=NA,
					neutral.genetic.targets=NA,
					b=curr.sol.MTX
				)
			}
		)
	},
	.parallel=TRUE
)
# kill cluster
clust <- stopCluster(clust)

cat("starting geographic surrogacy prioritizations\n")
geo.correlation.prioritisations <- llply(
	seq_along(unique(spp.samples.sub.DF$species)),
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
					NumberSolutions=rapr.params.LST[[MODE]]$surrogacy.analysis$optimal.replicates
				)
			}
		)
	},
	.progress='text',
	.parallel=FALSE
)

cat("starting random prioritizations for the geographic surrogate prioritizations \n")
clust <- makeCluster(general.params.LST[[MODE]]$threads, type='SOCK')
clusterEvalQ(clust, {library(rapr)})
clusterExport(clust, c('spp.samples.sub.DF','grid.sub.DF','rapr.params.LST',
	'MODE', 'species.prioritisation', 'ru', 'geo.correlation.prioritisations'))
registerDoParallel(clust)
geo.random.prioritisations <- llply(
	seq_along(unique(spp.samples.sub.DF$species)),
	function(i) {
		llply(
			seq_along(rapr.params.LST[[MODE]]$surrogacy.analysis$surrogate.target),
			function(j) {
				# extract number of planning units
				setMKLthreads(1)
				curr.optim_n <- table(rowSums(geo.correlation.prioritisations[[i]][[j]]@results@selections))
				curr.optim_p <- curr.optim_n / sum(curr.optim_n)
				# identify which planning units are occupied by the species
				curr.pu <- which(grid.sub.DF[[unique(spp.samples.sub.DF$species)[[i]]]]==1)
				zeros <- rep(0, length=nrow(grid.sub.DF))
				# generate portfolio of random selections
				curr.random_n <- as.numeric(sample(
					names(curr.optim_n),
					size=rapr.params.LST[[MODE]]$surrogacy.analysis$random.replicates,
					replace=TRUE, prob=as.numeric(curr.optim_p)
				))
				curr.sol.MTX<-t(sapply(
					curr.random_n,
					function(n) {replace(zeros, sample(x=curr.pu, size=n), rep(1, n))}
				))
				# generate RapSolved object
				species.prioritisation(
					x=spp.subset(ru, i),
					amount.targets=0.0001,
					env.surrogate.targets=NA,
					geo.surrogate.targets=NA,
					adaptive.genetic.targets=NA,
					neutral.genetic.targets=NA,
					b=curr.sol.MTX
				)
			}
		)
	},
	.parallel=TRUE
)
# kill cluster
clust <- stopCluster(clust)

# extract results
env.correlation.DF <- ldply(
	seq_along(env.correlation.prioritisations), 
	function(i) {
		mutate(
			rbind(
				mutate(
					ldply(env.correlation.prioritisations[[i]], extractResults),
					Surrogate.target=rep(
						rapr.params.LST[[MODE]]$surrogacy.analysis$surrogate.target,
						each=rapr.params.LST[[MODE]]$surrogacy.analysis$optimal.replicates
					),
					Method='Optimal'
				),
				mutate(
					ldply(env.random.prioritisations[[i]], extractResults),
					Surrogate.target=rep(
						rapr.params.LST[[MODE]]$surrogacy.analysis$surrogate.target,
						each=rapr.params.LST[[MODE]]$surrogacy.analysis$random.replicates
					),
					Method='Random'
				)
			),
			Type='Environmental',
			genetic.held=adaptive.held
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
			rbind(
				mutate(
					ldply(geo.correlation.prioritisations[[i]], extractResults),
					Surrogate.target=rep(
						rapr.params.LST[[MODE]]$surrogacy.analysis$surrogate.target,
						each=rapr.params.LST[[MODE]]$surrogacy.analysis$optimal.replicates
					),
					Method='Optimal'
					),
				mutate(
					ldply(geo.random.prioritisations[[i]], extractResults),
					Surrogate.target=rep(
						rapr.params.LST[[MODE]]$surrogacy.analysis$surrogate.target,
						each=rapr.params.LST[[MODE]]$surrogacy.analysis$random.replicates
					),
					Method='Random'
				)
			),
			Type='Geographic',
			genetic.held=neutral.held
		)
	}
) %>% mutate(
	genetic.held=replace(genetic.held, which(genetic.held<0), 0),
	adaptive.held=replace(adaptive.held, which(adaptive.held<0), 0),
	neutral.held=replace(neutral.held, which(neutral.held<0),0)
)
correlation.DF <- rbind(env.correlation.DF, geo.correlation.DF)

## save .rda
save.session('data/results/07-surrogacy-prioritizations.rda', compress='xz')

