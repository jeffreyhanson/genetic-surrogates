## load .rda
session::restore.session('results/.cache/02-surrogate-data.rda')

## load parameters
adegenet.params.LST <- parseTOML('parameters/adegenet.toml')

### adegenet analyses
## run analyses
clust <- makeCluster(adegenet.params.LST[[MODE]]$threads, type='SOCK')
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
				stat = adegenet.params.LST[[MODE]]$stat,
				choose.n.clust = adegenet.params.LST[[MODE]]$choose.n.clust,
				criterion = adegenet.params.LST[[MODE]]$criterion,
				max.n.clust = adegenet.params.LST[[MODE]]$max.n.clust,
				n.iter = adegenet.params.LST[[MODE]]$n.iter,
				n.start = adegenet.params.LST[[MODE]]$n.start,
				pca.select = adegenet.params.LST[[MODE]]$pca.select,
				perc.pca = adegenet.params.LST[[MODE]]$perc.pca
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
		print(i)
		# get population identities
		ids <- spp.Adegenet.LST[[i]]$grp
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
save.session('results/.cache/03-adegenet-analysis.rda')
