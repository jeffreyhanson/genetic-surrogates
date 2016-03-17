## load .rda
session::restore.session('results/.cache/05-genetic-nmds.rda')

# generate attribute spaces for geographic and environmental data
surrogate.ASL <- llply(
	list(grep('^env.*$', names(grid.DF)),grep('^geo.*$', names(grid.DF))),
	.fun=function(x) {
		make.multi.species.AttributeSpace(
			site.data=grid.DF[,x,drop=FALSE],
			species.data=grid.DF[,unique(spp.samples.DF$species),drop=FALSE],
			distance.metric='minkowski'
		)
	}
)

# generate attribute spaces for genetic data
adaptive.ASL <- llply(
	seq_along(unique(spp.samples.DF$species)),
	function(i) {
		# if no adaptive space for this species then return null
		if (ncol(select(grid.DF, contains(paste0(unique(spp.samples.DF$species)[i], '_adaptive'))))==0)
			return(NULL)
		# else return attribute space
		make.single.species.AttributeSpace(
			site.data=select(grid.DF, contains(paste0(unique(spp.samples.DF$species)[i], '_adaptive'))),
			species.data=na.omit(select(grid.DF, contains(paste0(unique(spp.samples.DF$species)[i], '_adaptive')))),
			spp.pos=i,
			n.species=n_distinct(spp.samples.DF$species),
			distance.metric='minkowski'
		)
	}
)
adaptive.ASL <- adaptive.ASL[!sapply(adaptive.ASL, is.null)]

neutral.ASL <- llply(
	seq_along(unique(spp.samples.DF$species)),
	function(i) {
		# if no adaptive space for this species then return null
		if (ncol(select(grid.DF, contains(paste0(unique(spp.samples.DF$species)[i], '_neutral'))))==0)
			return(NULL)
		# else return attribute space
		make.single.species.AttributeSpace(
			site.data=select(grid.DF, contains(paste0(unique(spp.samples.DF$species)[i], '_neutral'))),
			species.data=na.omit(select(grid.DF, contains(paste0(unique(spp.samples.DF$species)[i], '_neutral')))),
			spp.pos=i,
			n.species=n_distinct(spp.samples.DF$species),
			distance.metric='minkowski'
		)
	}
)
neutral.ASL <- neutral.ASL[!sapply(neutral.ASL, is.null)]

# make table with targets
target.DF <- make.targets(
	species=unique(spp.samples.DF$species),
	environmental.space=surrogate.ASL[[1]], geographic.space=surrogate.ASL[[2]],
	adaptive.spaces=adaptive.ASL, neutral.spaces=neutral.ASL,
	amount.target=0.2, space.target=0.2
)

# make Rap objects
rd <- RapData(
	polygon=SpatialPolygons2PolySet(grid.PLY),
	pu=data.frame(
		cost=grid.DF$pop.density,
		area=rep(1, nrow(grid.DF)),
		status=rep(0L, nrow(grid.DF))
	),
	species=data.frame(name=unique(spp.samples.DF$species)),
	target=target.DF,
	attribute.spaces=append(append(surrogate.ASL, adaptive.ASL), neutral.ASL),
	pu.species.probabilities=ldply(
		seq_along(unique(spp.samples.DF$species)),
		.fun=function(i) {
			data.frame(
				species=i,
				pu=which(grid.DF[[unique(spp.samples.DF$species)[i]]]==1),
				value=1
			)
		}
	),
	boundary=calcBoundaryData(grid.PLY)
)

# create RapUnsolved without cost data
ru <- RapUnsolved(RapUnreliableOpts(), rd)
ru@data@pu$cost <- 1

# create RapUnsolved with  cost data
ru_with_cost <- RapUnsolved(RapUnreliableOpts(), rd)

## save .rda
save.session('results/.cache/06-format-data-for-prioritisations.rda', compress='xz')
