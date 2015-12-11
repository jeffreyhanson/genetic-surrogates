# assign cells as labels
spp.BayeScanData.LST <- llply(
	seq_along(unique(spp.samples.DF$species)),
	function(i) {
		bd <- spp.BayeScanData.LST[[i]]
		sample.labels(bd) <- filter(spp.samples.DF, species==unique(spp.samples.DF$species)[i])$cell
		return(bd)
	}
)

# subset out polymorphisms with really low or really high frequency
spp.BayeScanData.LST <- llply(
	spp.BayeScanData.LST,
	function(x) {
		freqs <- colMeans(x@matrix)
		valid.loci <- which(freqs <= 1-(bs.freq) | freqs >= bs.freq)
		return(subset.loci(x, valid.loci))
	}
)


# run BayeScan
spp.BayeScan.LST <- llply(
	spp.BayeScanData.LST,
	run.BayeScan,
	threshold=bs.threshold,
	threads=bs.threads,
	n=bs.n,
	thin=bs.thin,
	nbp=bs.nbp,
	pilot=bs.pilot,
	burn=bs.burn
)

# run MDS
spp.mds.LST <- llply(
	spp.BayeScan.LST,
	function(i) {
		`names<-`(llply(c('adaptive', 'neutral'), function(j) {
			if (sum(i@results@fst==j)==0)
				return(NULL)
			return(
				mds(
					i,
					metric='gower',
					type=j,
					k=mds.k,
					trymax=mds.trymax
				)
			)
		}), c('adaptive','neutral'))
	}
)

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

