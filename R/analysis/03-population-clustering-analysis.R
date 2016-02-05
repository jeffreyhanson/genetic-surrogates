## load .rda
session::restore.session('results/.cache/02-surrogate-data.rda')

## load parameters
adegenet.params.LST <- parseTOML('parameters/adegenet.toml')

### adegenet analyses
# run clustering analyses
## run analyses
clust <- makeCluster(adegenet.params.LST[[MODE]]$find.clusters$threads, type='SOCK')
clusterEvalQ(clust, {library(structurer);library(adegenet)})
clusterExport(clust, c('adegenet.params.LST','MODE', 'spp.StructureData.LST', 'spp.samples.DF'))
registerDoParallel(clust)
spp.Adegenet.LST <- llply(
	seq_along(spp.StructureData.LST),
	.fun=function(i) {
		# run adegenet analysis
		return(
			find.clusters(
				suppressWarnings(df2genind(spp.StructureData.LST[[i]]@matrix, ploidy=1, type="PA")),
				stat = adegenet.params.LST[[MODE]]$find.clusters$stat,
				choose.n.clust = adegenet.params.LST[[MODE]]$find.clusters$choose.n.clust,
				criterion = adegenet.params.LST[[MODE]]$find.clusters$criterion,
				max.n.clust = adegenet.params.LST[[MODE]]$find.clusters$max.n.clust,
				n.iter = adegenet.params.LST[[MODE]]$find.clusters$n.iter,
				n.start = adegenet.params.LST[[MODE]]$find.clusters$n.start,
				pca.select = adegenet.params.LST[[MODE]]$find.clusters$pca.select,
				perc.pca = adegenet.params.LST[[MODE]]$find.clusters$perc.pca
			)
		)
	},
	.parallel=TRUE
)
clust <- stopCluster(clust)

# run dapc to compute membership probabilities




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
