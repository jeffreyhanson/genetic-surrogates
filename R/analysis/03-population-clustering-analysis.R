## load .rda
session::restore.session('results/.cache/02-surrogate-data.rda')

## load parameters
mclust.params.LST <- parseTOML('parameters/mclust.toml')
nmds.params.LST <- parseTOML('parameters/nmds.toml')

### mclust analyses
## run analyses
clust <- makeCluster(mclust.params.LST[[MODE]]$threads, type='SOCK')
clusterEvalQ(clust, {library(structurer);library(cluster);library(mclust);library(vegan)})
clusterExport(clust, c('nmds.params.LST','mclust.params.LST','MODE', 'spp.StructureData.LST', 'spp.samples.DF'))
registerDoParallel(clust)
spp.Mclust.LST <- llply(
	seq_along(spp.StructureData.LST),
		.fun=function(i) {
		## generate ordination
		curr.nmds <- bayescanr::nmds(
			bayescanr::BayeScanData(
				spp.StructureData.LST[[i]]@matrix,
				primers=spp.StructureData.LST[[i]]@loci.names,
				populations=rep(1, length(spp.StructureData.LST[[i]]@matrix)),
				labels=spp.StructureData.LST[[i]]@sample.names
			),
			metric='gower',
			max.stress=nmds.params.LST[[MODE]]$max.stress,
			min.k=nmds.params.LST[[MODE]]$min.k,
			max.k=nmds.params.LST[[MODE]]$max.k,
			trymax=nmds.params.LST[[MODE]]$trymax
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
spp.BayeScanData.sample.subset.LST <- llply(
	seq_along(spp.BayeScanData.LST),
	.fun=function(i) {
		# get population identities
		probs<-spp.Mclust.LST[[i]]$mclust$z
		ids <- apply(probs, 1, which.max)
		# identify individuals with uncertain population membership
		ids[which(apply(probs, 1, function(x) {max(x) < mclust.params.LST[[MODE]]$probability.threshold}))] <- NA_integer_
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
save.session('results/.cache/03-population-clustering-analysis.rda')
