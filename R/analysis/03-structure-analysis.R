## load .rda
session::restore.session('results/.cache/02-surrogate-data.rda')

## load parameters
structure.params.LST <- parseTOML('parameters/structure.toml')
clumpp.params.LST <- parseTOML('parameters/clumpp.toml')

### Structure analyses
dir.create('results/.cache/structure')

## run analyses
clust <- makeCluster(structure.params.LST[[MODE]]$threads, type='SOCK')
clusterEvalQ(clust, {library(structurer)})
clusterExport(clust, c('structure.params.LST','clumpp.params.LST','MODE', 'spp.StructureData.LST', 'spp.samples.DF'))
registerDoParallel(clust)
spp.StructureCollection.LST <- llply(
	seq_along(spp.StructureData.LST),
	.fun=function(x) {
		curr.dir <- paste0('results/.cache/structure/',unique(spp.samples.DF$species)[i])
		dir.create(curr.dir)
		run.Structure(
			spp.StructureData.LST[[i]],
			NUMRUNS=structure.params.LST[[MODE]]$numruns,
			MAXPOPS=structure.params.LST[[MODE]]$k,
			BURNIN=structure.params.LST[[MODE]]$burnin,
			NUMREPS=structure.params.LST[[MODE]]$numreps,
			NOADMIX=structure.params.LST[[MODE]]$noadmix,
			ADMBURNIN=structure.params.LST[[MODE]]$admburnin,
			REPEATS=clumpp.params.LST[[MODE]]$repeats,
			M=clumpp.params.LST[[MODE]]$m,
			S=clumpp.params.LST[[MODE]]$s,
			dir=curr.dir,
			clean=FALSE,
			verbose=TRUE
		)
	},
	.parallel=TRUE
)
clust <- stopCluster(clust)

## assign populations
spp.BayeScanData.sample.subset.LST <- llply(
	seq_along(spp.BayeScanData.LST),
	.fun=function(i) {
		print(i)
		# get population identities
		ids <- sample.membership(spp.StructureCollection.LST[[i]], threshold=structure.params.LST[[MODE]]$probthresh)
		validPos <- which(!is.na(ids))
		# remove individuals below threshold
		curr.spp <- spp.BayeScanData.LST[[i]]
		curr.spp <- bayescanr:::sample.subset.BayeScanData(curr.spp, validPos)
		curr.spp@labels <- filter(spp.samples.DF, species==unique(spp.samples.DF$species)[i])$cell[validPos]
		curr.spp@populations <- as.character(ids[validPos])
		return(curr.spp)
	}
)

## load .rda
save.session('results/.cache/03-stucture-analysis.rda')
