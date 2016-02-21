## load .rda
session::restore.session('results/.cache/02-surrogate-data.rda')

## load parameters
mclust.params.LST <- parseTOML('parameters/mclust.toml')
nmds.params.LST <- parseTOML('parameters/nmds.toml')

### mclust analyses
## run analyses
clust <- makeCluster(mclust.params.LST[[MODE]]$threads, type='SOCK')
clusterEvalQ(clust, {library(structurer);library(mclust);library(vegan)})
clusterExport(clust, c('nmds.params.LST','mclust.params.LST','MODE', 'spp.StructureData.LST', 'spp.samples.DF'))
registerDoParallel(clust)
spp.Mclust.LST <- llply(
	seq_along(spp.StructureData.LST),
		.fun=function(i) {
		## construct distance matrix
		curr.dist.MTX <- daisy(
			cbind(spp.StructureData.LST[[i]]@matrix,1),
			metric='gower',
			type=list(asymm=seq_len(ncol(spp.StructureData.LST[[i]]@matrix)+1))
		)
		## run mds with acceptable stress
		curr.stress <- 1.0
		curr.k <- nmds.params.LST[[MODE]]$min.k
		# find mds with suitable k
		while (curr.stress > nmds.params.LST[[MODE]]$max.stress & curr.k <= nmds.params.LST[[MODE]]$max.k) {
			curr.nmds <- metaMDS(
				comm=curr.dist.MTX,
				k=curr.k,
				trymax=nmds.params.LST[[MODE]]$trymax,
				wascores=FALSE,
				autotransform=FALSE,
				noshare=FALSE
			)
			curr.stress <- curr.nmds$stress
			curr.k <- curr.k + 1
		}
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
