## load .rda
session::restore.session('data/intermediate/04-outlier-locus-analysis.rda')

## load parameters
nmds.params.LST <- parseTOML('code/parameters/nmds.toml')

# init snow cluster
clust <- makeCluster(general.params.LST[[MODE]]$threads, type='PSOCK', outfile="")
clusterEvalQ(clust, {library(structurer);library(bayescanr);library(cluster);library(plyr);library(vegan)})
clusterExport(clust, c('MODE', 'spp.OutlierDetectionResults.LST', 'spp.OutlierDetectionData.LST', 'spp.StructureData.LST', 'nmds.params.LST'))
registerDoParallel(clust)

# run mds
spp.nmds.LST <- llply(
	seq_along(spp.OutlierDetectionData.LST),
	.fun=function(i) {
		# subset loci for species
		cat('starting species',i,'\n')
		if (is.null(spp.OutlierDetectionData.LST[[i]])) {
			# only 1 population in this species
			return(list('adaptive'=NULL, 'neutral'=NULL))
		}
		# manually classify loci as neutral or adaptive
		curr.spp <- bayescanr::BayeScanData(
			spp.StructureData.LST[[i]]@matrix,
			primers=spp.StructureData.LST[[i]]@loci.names,
			populations=rep('1', nrow(spp.StructureData.LST[[i]]@matrix)),
			labels=spp.StructureData.LST[[i]]@sample.names
		)
		outlier.pos <- which(spp.StructureData.LST[[i]]@loci.names %in% spp.OutlierDetectionData.LST[[i]]@primers[spp.OutlierDetectionResults.LST[[i]]])
		curr.spp.type <- replace(
			rep('neutral', ncol(spp.StructureData.LST[[i]]@matrix)),
			outlier.pos,
			'adaptive'
		)
	
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
save.session('data/intermediate/05-genetic-nmds.rda', compress='xz')
