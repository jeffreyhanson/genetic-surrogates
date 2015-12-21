## load .rda
session::restore.session('results/.cache/00-initialization.rda')

## compile spatial grid data
# load grid cell centroids
grid.DF <- fread(
	'data/Data_Meirmans_et_al_IntrabioDiv/ReadMe.txt',
	data.table=FALSE,
	skip='cell\tLong\tLat'
) %>% rename(
		grid.longitude=Long,
		grid.latitude=Lat
) %>% mutate(
	id=seq_along(grid.latitude)
)

# load in aflp data
spp.aflp.paths <- dir(
	'data/Data_Meirmans_et_al_IntrabioDiv',
	'^.*AFLP\\.dat$',
	full.names=TRUE
)[seq_len(n.spp)]

spp.BayeScanData.LST <- llply(
	spp.aflp.paths,
	read.BayeScanData
)

spp.StructureData.LST <- llply(
	spp.aflp.paths,
	read.StructureData
)

## compile species occurence data
# load in data
spp.loc.paths <- dir(
	'data/Data_Meirmans_et_al_IntrabioDiv',
	'^.*locations\\.txt$',
	full.names=TRUE
)[seq_len(n.spp)]
spp.samples.DF <- ldply(
	seq_along(spp.loc.paths),
	.fun=function(i) {
		x <- mutate(
			fread(spp.loc.paths[i], data.table=FALSE),
			species=gsub('_locations.txt', '', basename(spp.loc.paths[i]), fixed=TRUE)
		) %>% rename(
			cell=population,
			sample.longitude=longitude,
			sample.latitude=latitude
		)
		return(x[as.numeric(spp.BayeScanData.LST[[i]]@populations),])
	}
) %>% left_join(
		grid.DF,
		by='cell'
)
# append species data to grid data.frame (wide-format)
for (i in unique(spp.samples.DF$species))
	grid.DF[[i]] <- replace(
		rep(0, nrow(grid.DF)),
		which(grid.DF$cell %in% filter(spp.samples.DF, species==i)$cell),
		1
	)
 
## save .rda
save.session('results/.cache/01-load-data.rda')
