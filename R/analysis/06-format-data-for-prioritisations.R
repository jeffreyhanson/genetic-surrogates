## load .rda
session::restore.session('results/.cache/05-genetic-nmds.rda')

# subset data.frames for species with identifiable adaptive and/or neutral genetic variation
missing.species <- unique(spp.samples.DF$species)[laply(spp.BayeScanData.sample.subset.LST, function(x) {!is.null(x)})]
if (length(missing.species)>0) { 
	grid.sub.DF <- grid.DF
	for (i in missing.species)
		grid.sub.DF <- grid.sub.DF[, names(grid.sub.DF)[(-1 * grep(paste0('^.*',i,'.*$'), names(grid.sub.DF)))]]
	spp.samples.sub.DF <- filter(spp.samples.DF, !species %in% missing.species)
	grid.sub.PLY <- grid.PLY[!grid.PLY$species %in% missing.species,]
} else {
	grid.sub.DF <- grid.DF
	grid.sub.PLY <- grid.PLY
	spp.samples.sub.DF <- spp.samples.DF
}

# generate attribute spaces for geographic and environmental data
surrogate.ASL <- llply(
	list(grep('^env.*$', names(grid.sub.DF)),grep('^geo.*$', names(grid.sub.DF))),
	.fun=function(x) {
		make.multi.species.AttributeSpace(
			site.data=grid.sub.DF[,x,drop=FALSE],
			species.data=grid.sub.DF[,unique(spp.samples.sub.DF$species),drop=FALSE],
			distance.metric='euclidean'
		)
	}
)

# generate attribute spaces for genetic data
adaptive.ASL <- llply(
	seq_along(unique(spp.samples.sub.DF$species)),
	function(i) {
		# if no adaptive space for this species then return null
		if (ncol(select(grid.sub.DF, contains(paste0(unique(spp.samples.sub.DF$species)[i], '_adaptive'))))==0)
			return(NULL)
		# else return attribute space
		make.single.species.AttributeSpace(
			site.data=select(grid.sub.DF, contains(paste0(unique(spp.samples.sub.DF$species)[i], '_adaptive'))),
			species.data=na.omit(select(grid.sub.DF, contains(paste0(unique(spp.samples.sub.DF$species)[i], '_adaptive')))),
			spp.pos=i,
			n.species=n_distinct(spp.samples.sub.DF$species),
			distance.metric='euclidean'
		)
	}
)
adaptive.ASL <- adaptive.ASL[!sapply(adaptive.ASL, is.null)]

neutral.ASL <- llply(
	seq_along(unique(spp.samples.sub.DF$species)),
	function(i) {
		# if no neutral space for this species then return null
		if (ncol(select(grid.sub.DF, contains(paste0(unique(spp.samples.sub.DF$species)[i], '_neutral'))))==0)
			return(NULL)
		# else return attribute space
		make.single.species.AttributeSpace(
			site.data=select(grid.sub.DF, contains(paste0(unique(spp.samples.sub.DF$species)[i], '_neutral'))),
			species.data=na.omit(select(grid.sub.DF, contains(paste0(unique(spp.samples.sub.DF$species)[i], '_neutral')))),
			spp.pos=i,
			n.species=n_distinct(spp.samples.sub.DF$species),
			distance.metric='euclidean'
		)
	}
)
neutral.ASL <- neutral.ASL[!sapply(neutral.ASL, is.null)]

# make table with targets
target.DF <- make.targets(
	species=unique(spp.samples.sub.DF$species),
	environmental.space=surrogate.ASL[[1]], geographic.space=surrogate.ASL[[2]],
	adaptive.spaces=adaptive.ASL, neutral.spaces=neutral.ASL,
	amount.target=0.2, space.target=0.2
)

# make Rap objects
rd <- RapData(
	polygon=SpatialPolygons2PolySet(grid.PLY),
	pu=data.frame(
		cost=grid.sub.DF$pop.density,
		area=rep(1, nrow(grid.sub.DF)),
		status=rep(0L, nrow(grid.sub.DF))
	),
	species=data.frame(name=unique(spp.samples.sub.DF$species)),
	target=target.DF,
	attribute.spaces=append(append(surrogate.ASL, adaptive.ASL), neutral.ASL),
	pu.species.probabilities=ldply(
		seq_along(unique(spp.samples.sub.DF$species)),
		.fun=function(i) {
			data.frame(
				species=i,
				pu=which(grid.sub.DF[[unique(spp.samples.sub.DF$species)[i]]]==1),
				value=1
			)
		}
	),
	boundary=calcBoundaryData(grid.sub.PLY)
)

# create RapUnsolved without cost data
ru <- RapUnsolved(RapUnreliableOpts(), rd)
ru@data@pu$cost <- 1

# create RapUnsolved with  cost data
ru_with_cost <- RapUnsolved(RapUnreliableOpts(), rd)

## save .rda
save.session('results/.cache/06-format-data-for-prioritisations.rda', compress='xz')
