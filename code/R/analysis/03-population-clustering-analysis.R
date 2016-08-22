## load .rda
checkpoint::checkpoint('2016-08-03', R.version='3.3.1', scanForPackages=FALSE)
session::restore.session('data/intermediate/02-surrogate-data.rda')

## load parameters
structure.params.LST <- parseTOML('code/parameters/structure.toml')
clumpp.params.LST <- parseTOML('code/parameters/clumpp.toml')

### stucture analyses
# run structure analyses assuming population numbers from previous analysis
spp.StructureCollection.LST <- llply(
	seq_along(spp.StructureData.LST),
	function(i) {
		## init
		curr.spp.dir <- file.path('data/intermediate/structure', unique(spp.samples.DF$species)[i])
		curr.maxpops <- filter(spp.population.number.DF, species==unique(spp.samples.DF$species)[i])$K
		dir.create(curr.spp.dir, showWarnings=FALSE, recursive=TRUE)
		## run structure analysis and return results
		return(
			run.single.Structure(
				spp.StructureData.LST[[i]], MAXPOPS = curr.maxpops, NUMRUNS = structure.params.LST[[MODE]]$numruns, 
				BURNIN = structure.params.LST[[MODE]]$burnin, NUMREPS = structure.params.LST[[MODE]]$numreps, NOADMIX = structure.params.LST[[MODE]]$noadmix, 
				FREQSCORR = structure.params.LST[[MODE]]$freqscorr, ADMBURNIN = structure.params.LST[[MODE]]$admburnin, UPDATEFREQ=structure.params.LST[[MODE]]$updatefreq,
				M = clumpp.params.LST[[MODE]]$M, W = clumpp.params.LST[[MODE]]$W, S = clumpp.params.LST[[MODE]]$S,  REPEATS = clumpp.params.LST[[MODE]]$repeats, 
				dir = curr.spp.dir, clean = FALSE, verbose = FALSE, threads=general.params.LST[[MODE]]$threads 
			)
		)
	}
)

## load .rda
save.session('data/intermediate/03-population-clustering-analysis.rda', compress='xz')
