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
		initial.file.names1 <- paste0(unique(spp.samples.DF$species)[i], '_initial', c(".loadings", ".scores", ".zscores", ".maf", ".sigma"))
		initial.file.names2 <- paste0(curr.dir, '/initial', c(".loadings", ".scores", ".zscores", ".maf", ".sigma"))
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
		curr.K <- pcadapt.params.LST[[MODE]]$initial.K
		initial.run <- try(stop(),silent=TRUE)
		while(inherits(initial.run, 'try-error')) {
			curr.K <- curr.K - 1
			initial.run <- try(pcadapt(curr.spp, K=as.numeric(curr.K), ploidy=1, transpose=TRUE,
				method='mahalanobis', data.type='genotype', min.maf=pcadapt.params.LST[[MODE]]$min.maf,
				clean.files=FALSE, output.filename=paste0(unique(spp.samples.DF$species)[i],'_initial')), silent=TRUE)
		}
		file.copy(initial.file.names1, initial.file.names2, overwrite=TRUE)
		file.remove(initial.file.names1)
		
		# choose apropriate K
		prop.var.explained <- initial.run$singular.values/sum(initial.run$singular.values)
		curr.spp.K <- which(cumsum(prop.var.explained) > pcadapt.params.LST[[MODE]]$min.variance.explained)[1]
		
		# run second pica with minimum number of principle components needed to secure a acceptable level of variation
		inference.run <- pcadapt(curr.spp, K=as.numeric(curr.spp.K), ploidy=1, transpose=TRUE,
			method='mahalanobis', data.type='genotype', min.maf=pcadapt.params.LST[[MODE]]$min.maf,
			clean.files=FALSE, output.filename=paste0(unique(spp.samples.DF$species)[i],'_inference'))
		file.copy(inference.file.names1, inference.file.names2, overwrite=TRUE)
		file.remove(inference.file.names1)
		
		# calculate pvalues
		pvalues.DBL <- inference.run$pvalues
		qvalues.LST <- qvalue(pvalues.DBL)
		outliers <- which(qvalues.LST$qvalues < pcadapt.params.LST[[MODE]]$alpha.level)

		# return list 
		return(
			list(
				K=curr.spp.K,
				initial.run=initial.run,
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
