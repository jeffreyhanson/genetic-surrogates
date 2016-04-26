## load .rda
session::restore.session('data/intermediate/03-population-clustering-analysis.rda')

## load parameters
bayescan.params.LST <- parseTOML('code/parameters/bayescan.toml')
pcadapt.params.LST <- parseTOML('code/parameters/pcadapt.toml')
nmds.params.LST <- parseTOML('code/parameters/nmds.toml')

# subset out loci with polymorphisms that have really low or really high frequency
spp.OutlierDetectionData.LST <- llply(
	spp.OutlierDetectionData.LST,
	function(x) {
		# skip if null
		if (is.null(x))
			return(NULL)
		# else remove abundant or rare loci
		freqs <- colMeans(x@matrix, na.rm=TRUE)
		valid.loci <- which(freqs <= 1-(bayescan.params.LST[[MODE]]$freq) | freqs >= bayescan.params.LST[[MODE]]$freq)
		return(bayescanr:::loci.subset(x, valid.loci))
	}
)

# run pcadapt over subsetted objects
spp.pcadapt.LST <- llply(
	seq_along(spp.OutlierDetectionData.LST),
	.fun=function(i) {
		# skip if null
		if (is.null(spp.OutlierDetectionData.LST[[i]]))
			return(NULL)
		## else run pcadapt
		# imput missing values
		curr.spp <- spp.OutlierDetectionData.LST[[i]]@matrix
		curr.spp <- apply(curr.spp, 2, function(v) {
			if (sum(is.na(v))==0)
				return(v)
			v[which(is.na(v))] <- mean(v, na.rm=TRUE)
			return(v)
		})

		# run first principle components to determine the number needed to secure a acceptable level of variation
		initial.pca <- corpca(curr.spp, K=pcadapt.params.LST[[MODE]]$initial.K, ploidy=1, scale=FALSE)
		prop.var.explained <- initial.pca$singular_values/sum(initial.pca$singular_values)
		curr.spp.K <- which(cumsum(prop.var.explained) > pcadapt.params.LST[[MODE]]$min.variance.explained)[1]
		
		# run second pica with minimum number of principle components needed to secure a acceptable level of variation
		inference.pca <- corpca(curr.spp, K=curr.spp.K, ploidy=1, scale=FALSE)
		
		# compute statistics from pcadapt
		stats.LST <- computeStats(
			data=curr.spp, res=inference.pca, method='mahalanobis', 
			nSNP=ncol(curr.spp), K=curr.spp.K, data.type='genotype', min.maf=0
		)
		
		# calculate pvalues
		pvalues.DBL <- pval(stats.LST, method='mahalanobis', K=curr.spp.K)
		qvalues.LST <- qvalue(pvalues.DBL, lambda=seq(max(0,min(pvalues.DBL,na.rm=TRUE)+1e-10), min(1,max(pvalues.DBL,na.rm=TRUE)-1e-10), length.out=50))
		outliers <- which(qvalues.LST$qvalues < pcadapt.params.LST[[MODE]]$alpha.level)

		# return list 
		return(
			list(
				K=curr.spp.K,
				initial.pca=initial.pca,
				inference.pca=inference.pca,
				stats=stats.LST,
				pvalues=pvalues.DBL,
				qvalues=qvalues.LST,
				adaptive.loci=outliers
			)
		)
	}
)

# run BayeScan over subsetted objects
dir.create('data/intermediate/bayescan')
spp.BayeScan.LST <- llply(
	seq_along(spp.OutlierDetectionData.LST),
	.fun=function(i) {
		# skip if null
		if (is.null(spp.OutlierDetectionData.LST[[i]]))
			return(NULL)
		# else run bayescan
		curr.dir <- paste0('data/intermediate/bayescan/',unique(spp.samples.DF$species)[i])
		dir.create(curr.dir)
		curr.spp <- run.BayeScan(
			spp.OutlierDetectionData.LST[[i]],
			fdr=bayescan.params.LST[[MODE]]$fdr,
			threads=general.params.LST[[MODE]]$threads,
			n=bayescan.params.LST[[MODE]]$n,
			thin=bayescan.params.LST[[MODE]]$thin,
			nbp=bayescan.params.LST[[MODE]]$nbp,
			pilot=bayescan.params.LST[[MODE]]$pilot,
			burn=bayescan.params.LST[[MODE]]$burn,
			dir=curr.dir,
			clean=FALSE
		)
		return(
			list(
				bayescan=curr.spp,
				adaptive.loci=which(curr.spp@results@summary$type=='adaptive')
			)
		)
	}
)

# extract loci that are detected in both analyses
spp.OutlierDetectionResults.LST <- llply(
	seq_along(spp.OutlierDetectionData.LST),
	.fun=function(i) {
		# skip if null
		if (is.null(spp.OutlierDetectionData.LST[[i]]))
			return(NULL)
		# extract intersecting results
		return(intersect(spp.BayeScan.LST[[i]]$adaptive.loci,spp.pcadapt.LST[[i]]$adaptive.loci))
	}
)

## save .rda
save.session('data/intermediate/04-outlier-locus-analysis.rda', compress='xz')
