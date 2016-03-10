## load .rda
session::restore.session('results/.cache/03-population-clustering-analysis.rda')

## load parameters
bayescan.params.LST <- parseTOML('parameters/bayescan.toml')
nmds.params.LST <- parseTOML('parameters/nmds.toml')

# subset out loci with polymorphisms that have really low or really high frequency
spp.BayeScanData.sample.loci.subset.LST <- llply(
	spp.BayeScanData.sample.subset.LST,
	function(x) {
		# skip if null
		if (is.null(x))
			return(NULL)
		# else remove abundant or rare loci
		freqs <- colMeans(x@matrix, na.rm=TRUE)
		valid.loci <- which(freqs <= 1-(bayescan.params.LST[[MODE]]$freq) | freqs >= bayescan.params.LST[[MODE]]$freq)
		return(bayescanr:::loci.subset(x, valid.loci))
	}
)

# run BayeScan over subsetted objects
dir.create('results/.cache/bayescan')
spp.BayeScan.sample.loci.subset.LST <- llply(
	seq_along(spp.BayeScanData.sample.loci.subset.LST),
	.fun=function(i) {
		# skip if null
		if (is.null(spp.BayeScanData.sample.loci.subset.LST[[i]]))
			return(NULL)
		# else run bayescan
		curr.dir <- paste0('results/.cache/bayescan/',unique(spp.samples.DF$species)[i])
		dir.create(curr.dir)
		run.BayeScan(
			spp.BayeScanData.sample.loci.subset.LST[[i]],
			fdr=bayescan.params.LST[[MODE]]$fdr,
			threads=general.params.LST[[MODE]]$threads,
			n=bayescan.params.LST[[MODE]]$n,
			thin=bayescan.params.LST[[MODE]]$thin,
			nbp=bayescan.params.LST[[MODE]]$nbp,
			pilot=bayescan.params.LST[[MODE]]$pilot,
			burn=bayescan.params.LST[[MODE]]$burn,
			dir=curr.dir,
			clean=FALSE
		)
	}
)

## save .rda
save.session('results/.cache/04-outlier-locus-analysis.rda', compress='xz')
