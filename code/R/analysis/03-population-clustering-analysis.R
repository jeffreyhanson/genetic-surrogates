## load .rda
session::restore.session('data/intermediate/02-surrogate-data.rda')

## load parameters
structure.params.LST <- parseTOML('code/parameters/structure.toml')

### stucture analyses
dir.create('data/intermediate/structure', showWarnings=FALSE, recursive=TRUE)
spp.StructureCollection.LST <- run.Structure(spp.StructureData.LST, MAXPOPS = structure.params.LST[[MODE]]$maxpops, NUMRUNS = structure.params.LST[[MODE]]$numruns, 
	BURNIN = structure.params.LST[[MODE]]$burnin, NUMREPS = structure.params.LST[[MODE]]$numreps, NOADMIX = structure.params.LST[[MODE]]$noadmix, 
	FREQSCORR = structure.params.LST[[MODE]]$freqscorr, ADMBURNIN = structure.params.LST[[MODE]]$admburnin, UPDATEFREQ=structure.params.LST[[MODE]]$updatefreq,
	M = "LargeKGreedy", W = TRUE, S = FALSE,  REPEATS = structure.params.LST[[MODE]]$repeats, 
	dir = file.path('data/intermediate/structure',unique(spp.samples.DF$species)), clean = FALSE, verbose = FALSE, threads=general.params.LST[[MODE]]$threads)

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
