## load .rda
session::restore.session('results/.cache/03-stucture-analysis.rda')

# subset out loci with polymorphisms that have really low or really high frequency
spp.BayeScanData.sample.loci.subset.LST <- llply(
	spp.BayeScanData.sample.subset.LST,
	function(x) {
		freqs <- colMeans(x@matrix)
		valid.loci <- which(freqs <= 1-(bs.freq) | freqs >= bs.freq)
		return(bayescanr:::loci.subset(x, valid.loci))
	}
)

# run BayeScan over subsetted objects
spp.BayeScan.sample.loci.subset.LST <- llply(
	spp.BayeScanData.sample.loci.subset.LST,
	run.BayeScan,
	fdr=bs.fdr,
	threads=bs.threads,
	n=bs.n,
	thin=bs.thin,
	nbp=bs.nbp,
	pilot=bs.pilot,
	burn=bs.burn
)

# run mds
spp.mds.LST <- llply(
	seq_along(spp.BayeScanData.LST),
	.fun=function(i) {
		# subset loci for species
		curr.spp <- bayescanr:::loci.subset(
			spp.BayeScanData.LST[[i]],
			which(spp.BayeScanData.LST[[i]]@primers %in% spp.BayeScan.sample.loci.subset.LST[[i]]@data@primers)
		)
		# manually classify loci as neutral or adaptive
		curr.spp.type <- replace(
			rep('neutral', bayescanr:::n.loci(curr.spp)),
			spp.BayeScan.sample.loci.subset.LST[[i]]@results@summary$type=='adaptive',
			'adaptive'
		)
		# run mds over neutral and/or adaptive loci
		return(`names<-`(llply(c('adaptive', 'neutral'), function(j) {
			if (sum(curr.spp.type==j)==0)
				return(NULL)
			return(
				mds(
					bayescanr:::loci.subset(curr.spp, curr.spp.type==j),
					metric='gower',
					k=mds.k,
					trymax=mds.trymax
				)
			)
		}), c('adaptive','neutral')))
	}
)

# remove NULL entries from MDS list

# store mds rotations for each sample
spp.samples.DF <- ldply(seq_along(unique(spp.samples.DF$species)), .fun=function(i) {
	x <- filter(spp.samples.DF, species==unique(spp.samples.DF$species)[i])
	for (j in c('adaptive', 'neutral')) {
		if (!is.null(spp.mds.LST[[i]][[j]])) {
			x <- cbind(
				x,
				`names<-`(
					as.data.frame(spp.mds.LST[[i]][[j]]$points),
					paste0(j,'_d',seq_len(mds.k))
				)
			)
		}
	}
	return(x)
})

# store mds average rotation for each grid
for (i in seq_along(unique(spp.samples.DF$species))) {
	for (j in c('adaptive', 'neutral')) {
		if(!is.null(spp.mds.LST[[i]][[j]])) {
			curr.sub <- filter(spp.samples.DF, species==unique(spp.samples.DF$species)[i])
			for (k in seq_len(mds.k)) {
				curr.vals <- tapply(
					curr.sub[[paste0(j,'_d',k)]],
					curr.sub$cell,
					FUN=mean
				)
				curr.pos <- match(names(curr.vals), grid.DF$cell)
				grid.DF[curr.pos,paste0(unique(spp.samples.DF$species)[i],'_',j,'_d',k)] <- curr.vals
			}
		}
	}
}

# update grid.PLY with additional attributes
grid.PLY@data <- grid.DF

## save .rda
save.session('results/.cache/04-bayescan-analysis.rda')
