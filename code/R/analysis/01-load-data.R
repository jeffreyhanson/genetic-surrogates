## load .rda
session::restore.session('data/intermediate/00-initialization.rda')

## compile spatial grid data
# subset species 
all.spp <- seq_len(general.params.LST[[MODE]]$n.spp)

# load grid cell centroids
grid.DF <- fread(
	'data/raw/Data_Meirmans_et_al_IntrabioDiv/README',
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
	'data/raw/Data_Meirmans_et_al_IntrabioDiv',
	'^.*AFLP\\.dat$',
	full.names=TRUE
)[all.spp]

spp.StructureData.LST <- llply(
	spp.aflp.paths,
	read.StructureData
)

## compile species occurence data
# load in data
spp.loc.paths <- dir(
	'data/raw/Data_Meirmans_et_al_IntrabioDiv',
	'^.*locations\\.txt$',
	full.names=TRUE
)[all.spp]
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
		return(x[as.numeric(spp.StructureData.LST[[i]]@sample.names),])
	}
) %>% left_join(
		grid.DF,
		by='cell'
)

## remove individuals that are all NAs
for (i in seq_along(spp.StructureData.LST)) {
	# find individuals that are all NAs
	curr.invalid <- which(rowSums(is.na(spp.StructureData.LST[[i]]@matrix))==ncol(spp.StructureData.LST[[i]]@matrix))
	# if any invalid then remove from objects
	if (length(curr.invalid)>0) {
		# get valid individuals
		curr.valid <- which(rowSums(is.na(spp.StructureData.LST[[i]]@matrix))!=ncol(spp.StructureData.LST[[i]]@matrix))
		# subset from StructureData
		spp.StructureData.LST[[i]] <- structurer:::sample.subset.StructureData(spp.StructureData.LST[[i]], curr.valid)
		# remove from spp.samples.DF
		spp.samples.DF <- spp.samples.DF[-which(spp.samples.DF$species==unique(spp.samples.DF$species)[i])[curr.invalid],]
	}
}

## append species data to grid data.frame (wide-format)
for (i in unique(spp.samples.DF$species))
	grid.DF[[i]] <- replace(
		rep(0, nrow(grid.DF)),
		which(grid.DF$cell %in% filter(spp.samples.DF, species==i)$cell),
		1
	)


## load in the number populations for each species
spp.population.number.DF <- fread('data/raw/Data_Meirmans_et_al_IntrabioDiv/NumberPopulationsPerSpecies.csv', data.table=FALSE)

## save .rda
save.session('data/intermediate/01-load-data.rda', compress='xz')
