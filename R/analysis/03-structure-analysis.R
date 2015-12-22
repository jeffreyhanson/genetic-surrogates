## load .rda
session::restore.session('results/.cache/02-surrogate-data.rda')


### Structure analyses
## run analyses
clust <- makeCluster(st.threads, type='SOCK')
clusterEvalQ(clust, {library(structurer)})
registerDoParallel(clust)
spp.StructureCollection.LST <- llply(
	spp.StructureData.LST,
	run.Structure,
	NUMRUNS=st.numruns,
	MAXPOPS=st.k,
	BURNIN=st.burnin,
	NUMREPS=st.numreps,
	NOADMIX=st.noadmix,
	ADMBURNIN=st.admburnin,
	REPEATS=cl.repeats,
	M=cl.m,
	S=cl.s,
	verbose=TRUE,
	.parallel=TRUE
)
clust <- stopCluster(clust)

## assign populations
spp.BayeScanData.sample.subset.LST <- llply(
	seq_along(spp.BayeScanData.LST),
	.fun=function(i) {
		print(i)
		# get population identities
		ids <- sample.membership(spp.StructureCollection.LST[[i]], threshold=st.probthresh)
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
