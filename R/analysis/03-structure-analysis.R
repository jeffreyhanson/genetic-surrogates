### Structure analyses
## run analyses
spp.StructureCollection.LST <- llply(
	spp.StructureData.LST,
	run.Structure,
	NUMRUNS=st.numruns,
	MAXPOPS=st.k,
	BURNIN=st.burnin,
	NUMREPS=st.numreps,
	NOADMIX=st.noadmix,
	ADMBURNIN=st.admburnin
)

## assign populations
for (i in seq_along(spp.BayeScanData.LST)) {
	# get population identities
	ids <- sample.membership(spp.StructureCollection.LST[[i]], threshold=st.probthresh)
	validPos <- which(!is.na(ids))
	# remove individuals below threshold
	curr.spp <- spp.BayeScanData.LST[[i]]
	curr.spp <- sample.subset(curr.spp, validPos)
	sample.labels(curr.spp) <- sample.pops(curr.spp)
	sample.pops(curr.spp) <- as.character(ids)
	spp.BayeScanData.LST[[i]] <- curr.spp
}


