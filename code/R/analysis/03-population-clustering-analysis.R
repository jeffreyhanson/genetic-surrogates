## load .rda
session::restore.session('data/results/02-surrogate-data.rda')

## load parameters
mclust.params.LST <- parseTOML('code/parameters/mclust.toml')
nmds.params.LST <- parseTOML('code/parameters/nmds.toml')

### mclust analyses
## run analyses
clust <- makeCluster(general.params.LST[[MODE]]$threads, type='SOCK')
clusterEvalQ(clust, {library(structurer);library(cluster);library(mclust);library(vegan)})
clusterExport(clust, c('nmds.params.LST','mclust.params.LST','MODE', 'spp.StructureData.LST', 'spp.samples.DF'))
registerDoParallel(clust)
spp.Mclust.LST <- llply(
	seq_along(spp.StructureData.LST),
		.fun=function(i) {
		## generate ordination
		setMKLthreads(1)
		curr.nmds <- bayescanr::nmds(
			bayescanr::BayeScanData(
				spp.StructureData.LST[[i]]@matrix,
				primers=spp.StructureData.LST[[i]]@loci.names,
				populations=rep('1', nrow(spp.StructureData.LST[[i]]@matrix)),
				labels=spp.StructureData.LST[[i]]@sample.names
			),
			metric='gower',
			max.stress=nmds.params.LST[[MODE]]$population.clustering$max.stress,
			min.k=nmds.params.LST[[MODE]]$population.clustering$min.k,
			max.k=nmds.params.LST[[MODE]]$population.clustering$max.k,
			trymax=nmds.params.LST[[MODE]]$population.clustering$trymax
		)
		## run mclust
		return(
			list(
				nmds=curr.nmds,
				mclust=Mclust(
					curr.nmds$points,
					G=mclust.params.LST[[MODE]]$G
				)
			)
		)
	},
	.parallel=TRUE
)
clust <- stopCluster(clust)

## assign populations
spp.OutlierDetectionData.LST <- llply(
	seq_along(spp.StructureData.LST),
	.fun=function(i) {
		# get population identities
		probs<-spp.Mclust.LST[[i]]$mclust$z
		# identify individuals with uncertain population membership
		ids <- apply(probs, 1, which.max)
		ids[which(apply(probs, 1, function(x) {max(x) < mclust.params.LST[[MODE]]$probability.threshold}))] <- NA_integer_
		validPos <- which(!is.na(ids))
		# if only one population return null
		if (n_distinct(ids)==1)
			return(NULL)
		# remove individuals below threshold
		curr.spp <- bayescanr::BayeScanData(spp.StructureData.LST[[i]]@matrix[validPos,,drop=FALSE],
			spp.StructureData.LST[[i]]@loci.names, as.character(ids[validPos]),
			spp.StructureData.LST[[i]]@sample.names[validPos])
		return(curr.spp)
	}
)

## load .rda
save.session('data/results/03-population-clustering-analysis.rda', compress='xz')
