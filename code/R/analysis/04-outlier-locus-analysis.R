## load .rda
checkpoint::checkpoint(general.params.LST[[MODE]]$checkpoint_date, R.version=general.params.LST[[MODE]]$checkpoint_R_version, scanForPackages=FALSE)
session::restore.session('data/intermediate/03-population-clustering-analysis.rda')

## load parameters
bayescan.params.LST <- parseTOML('code/parameters/bayescan.toml')
pcadapt.params.LST <- parseTOML('code/parameters/pcadapt.toml')
nmds.params.LST <- parseTOML('code/parameters/nmds.toml')

# assign populations
spp.OutlierDetectionData.LST <- llply(
	seq_along(spp.StructureCollection.LST),
	.fun=function(i) {
		# get population identities
		probs <- structurer:::sample.membership(spp.StructureCollection.LST[[i]])
		# identify individuals with uncertain population membership
		ids <- apply(probs, 1, which.max)
		ids[which(apply(probs, 1, function(x) {max(x) < structure.params.LST[[MODE]]$probability.threshold}))] <- NA_integer_
		validPos <- which(!is.na(ids))
		# remove individuals below threshold and use populations specified by structure
		curr.spp <- bayescanr::BayeScanData(spp.StructureData.LST[[i]]@matrix[validPos,,drop=FALSE],
			spp.StructureData.LST[[i]]@loci.names, as.character(ids[validPos]),
			spp.StructureData.LST[[i]]@sample.names[validPos])
		return(curr.spp)
	}
)

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
dir.create('data/intermediate/pcadapt', showWarnings=FALSE)
spp.pcadapt.LST <- llply(
	seq_along(spp.OutlierDetectionData.LST),
	.fun=function(i) {
		# skip if null
		if (is.null(spp.OutlierDetectionData.LST[[i]]))
			return(NULL)
		## else run pcadapt
		# init
		curr.dir <- paste0('data/intermediate/pcadapt/', unique(spp.samples.DF$species)[i]) 
		dir.create(curr.dir, showWarnings=FALSE, recursive=TRUE)
		inference.file.names1 <- paste0(unique(spp.samples.DF$species)[i], '_inference', c(".loadings", ".scores", ".zscores", ".maf", ".sigma"))
		inference.file.names2 <- paste0(curr.dir, '/inference', c(".loadings", ".scores", ".zscores", ".maf", ".sigma"))
		
		# set missing values
		curr.spp <- spp.OutlierDetectionData.LST[[i]]@matrix
		curr.spp <- apply(curr.spp, 2, function(v) {
			if (sum(is.na(v))==0)
				return(v)
			v[which(is.na(v))] <- mean(v, na.rm=TRUE)
			return(v)
		})

		# run initial analysis to determine suitable K
		curr.K <- apply(curr.spp, 2, paste, collapse=',') %>% n_distinct()
		initial.pca <- try(stop(),silent=TRUE)
		while(inherits(initial.pca, 'try-error')) {
			curr.K <- curr.K - 1
			initial.pca <- try(corpca(curr.spp, curr.K), silent=TRUE)
		}
		if (file.exists('tmp.pcadapt')) file.remove('tmp.pcadapt')
		
		# choose apropriate K
		prop.var.explained <- initial.pca$singular.values/sum(initial.pca$singular.values)
		curr.spp.K <- which(cumsum(prop.var.explained) > pcadapt.params.LST[[MODE]]$min.variance.explained)[1]
		
		# run second pica with minimum number of principle components needed to secure a acceptable level of variation
		inference.run <- pcadapt(curr.spp, K=as.numeric(curr.spp.K), ploidy=1, transpose=TRUE,
			method='mahalanobis', data.type='genotype', min.maf=pcadapt.params.LST[[MODE]]$min.maf,
			clean.files=FALSE, output.filename=paste0(unique(spp.samples.DF$species)[i],'_inference'))
		file.copy(inference.file.names1, inference.file.names2, overwrite=TRUE)
		file.remove(inference.file.names1)
		if (file.exists('tmp.pcadapt')) file.remove('tmp.pcadapt')
		
		# calculate pvalues
		pvalues.DBL <- inference.run$pvalues
		qvalues.LST <- qvalue(pvalues.DBL)
		outliers <- which(qvalues.LST$qvalues < pcadapt.params.LST[[MODE]]$alpha.level)

		# return list 
		return(
			list(
				K=curr.spp.K,
				initial.pca=initial.pca,
				inference.run=inference.run,
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
			reps=bayescan.params.LST[[MODE]]$reps,
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
