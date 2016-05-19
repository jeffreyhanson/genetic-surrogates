## load .rda
session::restore.session('data/intermediate/02-surrogate-data.rda')

## load parameters
structure.params.LST <- parseTOML('code/parameters/structure.toml')

### stucture analyses
## run analyses
clust <- makeCluster(max(floor(general.params.LST[[MODE]]$threads/structure.params.LST[[MODE]]$numruns),1), type="PSOCK", outfile="")
clusterEvalQ(clust, {library(structurer)})
clusterExport(clust, c('spp.samples.DF','spp.StructureData.LST', 'spp.populations.DF', 'structure.params.LST', 'MODE'))
registerDoParallel(clust)
spp.StructureCollection.LST <- llply(
	seq_along(spp.StructureData.LST),
		.fun=function(i) {
		## init
		curr.spp.dir <- file.path('data/intermediate/structure',unique(spp.samples.DF$species)[i])
		dir.create(curr.spp.dir, showWarnings=FALSE, recursive=TRUE)
		## run structure analysis and return results
		return(
			run.Structure(spp.StructureData.LST[[i]], MAXPOPS = structure.params.LST[[MODE]]$maxpops, NUMRUNS = structure.params.LST[[MODE]]$numruns, 
				BURNIN = structure.params.LST[[MODE]]$burnin, NUMREPS = structure.params.LST[[MODE]]$numreps, NOADMIX = structure.params.LST[[MODE]]$noadmix, 
				FREQSCORR = structure.params.LST[[MODE]]$freqscorr, ADMBURNIN = structure.params.LST[[MODE]]$admburnin, UPDATEFREQ=structure.params.LST[[MODE]]$updatefreq,
				M = "LargeKGreedy", W = TRUE, S = FALSE,  REPEATS = structure.params.LST[[MODE]]$repeats, 
				dir = curr.spp.dir, clean = FALSE, verbose = FALSE, threads=structure.params.LST[[MODE]]$numruns
			)
		)
	},
	.parallel=TRUE
)
stopCluster(clust)

## assign populations
spp.OutlierDetectionData.LST <- llply(
	seq_along(spp.StructureCollection.LST),
	.fun=function(i) {
		# get population identities
		probs <- structurer:::sample.membership(spp.StructureCollection.LST[[i]])
		# identify individuals with uncertain population membership
		ids <- apply(probs, 1, which.max)
		ids[which(apply(probs, 1, function(x) {max(x) < structure.params.LST[[MODE]]$probability.threshold}))] <- NA_integer_
		validPos <- which(!is.na(ids))
		# remove individuals below threshold
		curr.spp <- bayescanr::BayeScanData(spp.StructureData.LST[[i]]@matrix[validPos,,drop=FALSE],
			spp.StructureData.LST[[i]]@loci.names, as.character(ids[validPos]),
			spp.StructureData.LST[[i]]@sample.names[validPos])
		return(curr.spp)
	}
)

## load .rda
save.session('data/intermediate/03-population-clustering-analysis.rda', compress='xz')
