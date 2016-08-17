## load .rda
checkpoint::checkpoint(general.params.LST[[MODE]]$checkpoint_date, R.version=general.params.LST[[MODE]]$checkpoint_R_version, scanForPackages=FALSE)
session::restore.session('data/intermediate/05-genetic-nmds.rda')

# subset data.frames for species with identifiable adaptive and/or neutral genetic variation
missing.species2 <- unique(spp.samples.DF$species)[laply(spp.OutlierDetectionData.LST, function(x) {is.null(x)})]

if (length(missing.species2)>0) { 
	grid.sub.DF <- grid.DF
	grid.sub.PLY <- grid.PLY
	for (i in missing.species2) {
		cols <- names(grid.sub.DF)[(-1 * grep(paste0('^.*',i,'.*$'), names(grid.sub.DF)))]
		grid.sub.DF <- grid.sub.DF[,cols]
		grid.sub.PLY <- grid.sub.PLY[,cols]
	}
	spp.samples.sub.DF <- filter(spp.samples.DF, !species %in% missing.species2)
} else {
	grid.sub.DF <- grid.DF
	grid.sub.PLY <- grid.PLY
	spp.samples.sub.DF <- spp.samples.DF
}

# generate attribute spaces for geographic and environmental data
geographic.AS <- make.surrogate.AttributeSpaces(
	site.data=select(grid.sub.DF, starts_with('geo')),
	species.data=grid.sub.DF[,unique(spp.samples.sub.DF$species),drop=FALSE],
	name='geographic')

environmental.AS <- make.surrogate.AttributeSpaces(
	site.data=select(grid.sub.DF, starts_with('env')),
	species.data=grid.sub.DF[,unique(spp.samples.sub.DF$species),drop=FALSE],
	name='environmental')

# generate attribute spaces for genetic data
adaptive.AS <- llply(
	seq_along(unique(spp.samples.sub.DF$species)),
	function(i) {
		# if no neutral space for this species then return null
		if (ncol(select(grid.sub.DF, contains(paste0(unique(spp.samples.sub.DF$species)[i], '_adaptive'))))==0)
			return(NULL)
		# else return attribute space
		make.genetic.AttributeSpace(
			site.data=select(grid.sub.DF, contains(paste0(unique(spp.samples.sub.DF$species)[i], '_adaptive'))),
			species.data=na.omit(select(grid.sub.DF, contains(paste0(unique(spp.samples.sub.DF$species)[i], '_adaptive')))),
			species=i
		)
	}
)
adaptive.AS <- AttributeSpaces(spaces=adaptive.AS[!sapply(adaptive.AS, is.null)], name='adaptive')

neutral.AS <- llply(
	seq_along(unique(spp.samples.sub.DF$species)),
	function(i) {
		# if no neutral space for this species then return null
		if (ncol(select(grid.sub.DF, contains(paste0(unique(spp.samples.sub.DF$species)[i], '_neutral'))))==0)
			return(NULL)
		# else return attribute space
		make.genetic.AttributeSpace(
			site.data=select(grid.sub.DF, contains(paste0(unique(spp.samples.sub.DF$species)[i], '_neutral'))),
			species.data=na.omit(select(grid.sub.DF, contains(paste0(unique(spp.samples.sub.DF$species)[i], '_neutral')))),
			species=i
		)
	}
)
neutral.AS <- AttributeSpaces(spaces=neutral.AS[!sapply(neutral.AS, is.null)], name='neutral')

# make table with targets
target.DF <- make.targets(
	species=unique(spp.samples.sub.DF$species),
	environmental.space=environmental.AS, geographic.space=geographic.AS,
	adaptive.spaces=adaptive.AS, neutral.spaces=neutral.AS,
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
	attribute.spaces=list(environmental.AS, geographic.AS, adaptive.AS, neutral.AS),
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
save.session('data/intermediate/06-format-data-for-prioritisations.rda', compress='xz')
