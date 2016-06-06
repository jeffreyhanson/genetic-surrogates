## load .rda
session::restore.session('data/intermediate/02-surrogate-data.rda')

## load parameters
structure.params.LST <- parseTOML('code/parameters/structure.toml')
clumpp.params.LST <- parseTOML('code/parameters/clumpp.toml')

### stucture analyses
# run structure analyses and set K using Evanno's method as implemented in StructureHarvester
dir.create('data/intermediate/structure', showWarnings=FALSE, recursive=TRUE)
raw.spp.StructureCollection.LST <- run.Structure(spp.StructureData.LST, MAXPOPS = structure.params.LST[[MODE]]$maxpops, NUMRUNS = structure.params.LST[[MODE]]$numruns, 
	BURNIN = structure.params.LST[[MODE]]$burnin, NUMREPS = structure.params.LST[[MODE]]$numreps, NOADMIX = structure.params.LST[[MODE]]$noadmix, 
	FREQSCORR = structure.params.LST[[MODE]]$freqscorr, ADMBURNIN = structure.params.LST[[MODE]]$admburnin, UPDATEFREQ=structure.params.LST[[MODE]]$updatefreq,
	M = clumpp.params.LST[[MODE]]$M, W = clumpp.params.LST[[MODE]]$W, S = clumpp.params.LST[[MODE]]$S,  REPEATS = clumpp.params.LST[[MODE]]$repeats, 
	dir = file.path('data/intermediate/structure',unique(spp.samples.DF$species)), clean = FALSE, verbose = FALSE, threads=general.params.LST[[MODE]]$threads)

# check if the "truly" best K is actually 1
# this can also be detected if most of the individuals for the chosen K have similar probabilities for each population
spp.StructureCollection.LST <- llply(raw.spp.StructureCollection.LST, function(x) {
	# get population identities
	probs <- structurer:::sample.membership(x)
	# identify individuals with uncertain population membership
	ids <- apply(probs, 1, which.max)
	ids[which(apply(probs, 1, function(x) {max(x) < structure.params.LST[[MODE]]$probability.threshold}))] <- NA_integer_
	validPos <- which(!is.na(ids))
	# if over half of individuals have uncertain memberships then assume K=1
	if (length(validPos)/nrow(probs) < 0.5)
		x@best <- 1
	return(x)
})

## load .rda
save.session('data/intermediate/03-population-clustering-analysis.rda', compress='xz')
