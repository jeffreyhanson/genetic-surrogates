## load .rda
checkpoint::checkpoint('2016-11-26', R.version='3.3.2', scanForPackages=FALSE)
session::restore.session('data/intermediate/06-format-data-for-prioritisations.rda')

## load parameters
raptr.params.LST <- parseTOML('code/parameters/raptr.toml')
gurobi.params.LST <- parseTOML('code/parameters/gurobi.toml')

## surrogacy analysis
# generate prioritistions
cat("starting random prioritizations for the environmental surrogate prioritizations \n")
clust <- makeCluster(general.params.LST[[MODE]]$threads, type='PSOCK', outfile="")
clusterEvalQ(clust, {library(raptr)})
clusterExport(clust, c('spp.samples.sub.DF','grid.sub.DF','raptr.params.LST', 'MODE', 'species.prioritisation', 'ru'))
registerDoParallel(clust)
env.surrogacy.prioritisations <- llply(
	seq_along(unique(spp.samples.sub.DF$species)),
	function(i) {
		# identify which planning units are occupied by the species
		curr.pu <- which(grid.sub.DF[[unique(spp.samples.sub.DF$species)[[i]]]]==1)
		curr.n.occupied <- length(curr.pu)
		zeros <- rep(0, length=nrow(grid.sub.DF))
		# generate portfolio of random selections
		curr.random.n <- sample.int(length(curr.pu), raptr.params.LST[[MODE]]$surrogacy.analysis$random.replicates, replace=TRUE)
		curr.random.selections <- rbind.fill.matrix(lapply(seq_along(curr.random.n), function(i) {
			matrix(c(rep(i, curr.random.n[i]), sample(x=curr.pu, curr.random.n[i], replace=FALSE, prob=NULL)), ncol=2)
		}))
		curr.sol.MTX<-matrix(0, ncol=nrow(grid.sub.DF), nrow=raptr.params.LST[[MODE]]$surrogacy.analysis$random.replicates)
		curr.sol.MTX[curr.random.selections] <- 1
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
	},
	.parallel=TRUE
)
stopCluster(clust)

cat("starting random prioritizations for the geographic surrogate prioritizations \n")
clust <- makeCluster(general.params.LST[[MODE]]$threads, type='PSOCK', outfile="")
clusterEvalQ(clust, {library(raptr)})
clusterExport(clust, c('spp.samples.sub.DF','grid.sub.DF','raptr.params.LST', 'MODE', 'species.prioritisation', 'ru'))
registerDoParallel(clust)
geo.surrogacy.prioritisations <- llply(
	seq_along(unique(spp.samples.sub.DF$species)),
	function(i) {
		# identify which planning units are occupied by the species
		curr.pu <- which(grid.sub.DF[[unique(spp.samples.sub.DF$species)[[i]]]]==1)
		curr.n.occupied <- length(curr.pu)
		zeros <- rep(0, length=nrow(grid.sub.DF))
		# generate portfolio of random selections
		curr.random.n <- sample.int(length(curr.pu), raptr.params.LST[[MODE]]$surrogacy.analysis$random.replicates, replace=TRUE)
		curr.random.selections <- rbind.fill.matrix(lapply(seq_along(curr.random.n), function(i) {
			matrix(c(rep(i, curr.random.n[i]), sample(x=curr.pu, curr.random.n[i], replace=FALSE, prob=NULL)), ncol=2)
		}))
		curr.sol.MTX<-matrix(0, ncol=nrow(grid.sub.DF), nrow=raptr.params.LST[[MODE]]$surrogacy.analysis$random.replicates)
		curr.sol.MTX[curr.random.selections] <- 1
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
	},
	.parallel=TRUE
)
# kill cluster
clust <- stopCluster(clust)

# extract results
env.surrogacy.DF <- ldply(env.surrogacy.prioritisations, .fun=extractResults) %>% 
	mutate(
		Type='Environmental',
		genetic.held=adaptive.held,
		genetic.held=replace(genetic.held, which(genetic.held<0), 0),
		surrogate.held=environmental.held,
		surrogate.held=replace(surrogate.held, which(surrogate.held<0), 0)
	) %>% filter(!is.na(genetic.held))

geo.surrogacy.DF <- ldply(geo.surrogacy.prioritisations,extractResults) %>% 
	mutate(
		Type='Geographic',
		genetic.held=neutral.held,
		genetic.held=replace(genetic.held, which(genetic.held<0), 0),
		surrogate.held=geographic.held,
		surrogate.held=replace(surrogate.held, which(surrogate.held<0), 0)
)

surrogacy.DF <- rbind(env.surrogacy.DF, geo.surrogacy.DF)

## save .rda
save.session('data/intermediate/07-surrogacy-prioritizations.rda', compress='xz')

