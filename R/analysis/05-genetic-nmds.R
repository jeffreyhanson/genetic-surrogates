## load .rda
session::restore.session('results/.cache/04-outlier-locus-analysis.rda')

## load parameters
nmds.params.LST <- parseTOML('parameters/nmds.toml')

# init snow cluster
clust <- makeCluster(general.params.LST[[MODE]]$threads, type='SOCK')
clusterEvalQ(clust, {library(structurer);library(bayescanr);library(cluster);library(plyr);library(vegan)})
clusterExport(clust, c('MODE', 'spp.BayeScanData.LST', 'spp.BayeScan.sample.loci.subset.LST', 'nmds.params.LST'))
registerDoParallel(clust)

# run mds
spp.nmds.LST <- llply(
	seq_along(spp.BayeScanData.LST),
	.fun=function(i) {
		# subset loci for species
		cat('starting species',i,'\n')
		if (is.null(spp.BayeScan.sample.loci.subset.LST[[i]])) {
			# all loci are neutral since only 1 population
			curr.spp <- spp.BayeScanData.LST[[i]]
			# manually classify loci as neutral or adaptive
			curr.spp.type <- rep('neutral', bayescanr:::n.loci(curr.spp))
		} else {
			# extract loci
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
		}
		# run nmds over neutral and/or adaptive loci
		return(`names<-`(llply(c('adaptive', 'neutral'), function(j) {
			if (sum(curr.spp.type==j)==0)
				return(NULL)
			# return nmds
			return(
				curr.nmds <- bayescanr::nmds(
					bayescanr:::loci.subset(curr.spp, curr.spp.type==j),
					metric='gower',
					max.stress=nmds.params.LST[[MODE]]$max.stress,
					min.k=nmds.params.LST[[MODE]]$min.k,
					max.k=nmds.params.LST[[MODE]]$max.k,
					trymax=nmds.params.LST[[MODE]]$trymax
				)
			)
		}), c('adaptive','neutral')))
	},
	.parallel=TRUE
)

# kill cluster
clust <- stopCluster(clust)

# store mds rotations for each sample
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
save.session('results/.cache/05-genetic-nmds.rda', compress='xz')
