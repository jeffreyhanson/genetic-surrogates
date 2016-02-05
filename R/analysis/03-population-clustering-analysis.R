## load .rda
session::restore.session('results/.cache/02-surrogate-data.rda')

## load parameters
mclust.params.LST <- parseTOML('parameters/mclust.toml')

### mclust analyses
# run clustering analyses
clust <- makeCluster(mclust.params.LST[[MODE]]$threads, type='SOCK')
clusterEvalQ(clust, {library(structurer);library(mclust)})
clusterExport(clust, c('mclust.params.LST','MODE', 'spp.StructureData.LST', 'spp.samples.DF'))
registerDoParallel(clust)
spp.Mclust.LST <- llply(
	seq_along(spp.StructureData.LST),
	.fun=function(i) {
		# init
		curr.MTX <- spp.StructureData.LST[[i]]@matrix
		# impute missing data if apropriate
		if (any(is.na(curr.MTX)))
			curr.MTX <- imputeData(curr.MTX)
		# run cluster analysis
		return(
			Mclust(
				curr.MTX,
				G=mclust.params.LST[[MODE]]$G
			)
		)
	},
	.parallel=TRUE
)
clust <- stopCluster(clust)

# assign individuals to populations if they have probability to a single population above threshold
spp.BayeScanData.sample.subset.LST <- llply(
	seq_along(spp.BayeScanData.LST),
	.fun=function(i) {
		print(i)
		# get population identities
		probs <- spp.Mclust.LST[[i]]$z
		ids <- apply(probs, 1, which.max)
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
