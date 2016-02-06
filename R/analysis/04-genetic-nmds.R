## load .rda
session::restore.session('results/.cache/03-population-clustering-analysis.rda')

## load parameters
bayescan.params.LST <- parseTOML('parameters/bayescan.toml')
nmds.params.LST <- parseTOML('parameters/nmds.toml')

# subset out loci with polymorphisms that have really low or really high frequency
spp.BayeScanData.sample.loci.subset.LST <- llply(
	spp.BayeScanData.sample.subset.LST,
	function(x) {
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
		curr.dir <- paste0('results/.cache/bayescan/',unique(spp.samples.DF$species)[i])
		dir.create(curr.dir)
		run.BayeScan(
			spp.BayeScanData.sample.loci.subset.LST[[i]],
			fdr=bayescan.params.LST[[MODE]]$fdr,
			threads=bayescan.params.LST[[MODE]]$threads,
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

# init snow cluster
clust <- makeCluster(nmds.params.LST[[MODE]]$threads, type='SOCK')
clusterEvalQ(clust, {library(structurer);library(bayescanr);library(plyr)})
clusterExport(clust, c('MODE', 'spp.BayeScanData.LST', 'spp.BayeScan.sample.loci.subset.LST', 'nmds.params.LST'))
registerDoParallel(clust)

# run mds
spp.nmds.LST <- llply(
	seq_along(spp.BayeScanData.LST),
	.fun=function(i) {
		# subset loci for species
		cat('starting species',i,'\n')
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
		# run nmds over neutral and/or adaptive loci
		return(`names<-`(llply(c('adaptive', 'neutral'), function(j) {
			if (sum(curr.spp.type==j)==0)
				return(NULL)
			# init
			cat('\tstarting',j,'loci \n')
			curr.stress <- 1.0
			curr.k <- nmds.params.LST[[MODE]]$min.k
			# find mds with suitable k
			while (curr.stress > nmds.params.LST[[MODE]]$max.stress & curr.k <= nmds.params.LST[[MODE]]$max.k) {
				cat('\t\tk=',curr.k,'\n')
				curr.nmds <- mds(
					bayescanr:::loci.subset(curr.spp, curr.spp.type==j),
					metric='gower',
					k=curr.k,
					trymax=nmds.params.LST[[MODE]]$trymax,
					wascores=FALSE,
					autotransform=FALSE,
					noshare=FALSE
				)
				curr.stress <- curr.nmds$stress
				curr.k <- curr.k + 1
			}
			return(curr.nmds)
		}), c('adaptive','neutral')))
	},
	.parallel=TRUE
)

# kill cluster
clust <- stopCluster(clust)

# storen mds rotations for each sample
spp.samples.DF <- ldply(seq_along(unique(spp.samples.DF$species)), .fun=function(i) {
	x <- filter(spp.samples.DF, species==unique(spp.samples.DF$species)[i])
	for (j in c('adaptive', 'neutral')) {
		if (!is.null(spp.nmds.LST[[i]][[j]])) {
			x <- cbind(
				x,
				`names<-`(
					as.data.frame(spp.nmds.LST[[i]][[j]]$points),
					paste0(j,'_d',seq_len(spp.nmds.LST[[i]][[j]]$ndim))
				)
			)
		}
	}
	return(x)
})

# store nmds average rotation for each grid
for (i in seq_along(unique(spp.samples.DF$species))) {
	for (j in c('adaptive', 'neutral')) {
		if(!is.null(spp.nmds.LST[[i]][[j]])) {
			curr.sub <- filter(spp.samples.DF, species==unique(spp.samples.DF$species)[i])
			for (k in seq_len(spp.nmds.LST[[i]][[j]]$ndim)) {
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
save.session('results/.cache/04-genetic-nmds.rda')
