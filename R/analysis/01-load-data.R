## load .rda
session::restore.session('results/.cache/00-initialization.rda')

# compile spatial grid data
all.spp <- seq_len(general.params.LST[[MODE]]$n.spp)

### load data
## load grid cell centroids
grid.DF <- 'data/Taberlet_etal_EcologyLetters2012/FloristicData/IBDcoordinates.csv' %>% 	
	fread(,data.table=FALSE) %>%
	select(GRID_CELL, LON, LAT) %>%
	rename(cell=GRID_CELL, grid.longitude=LON, grid.latitude=LAT) %>%
	mutate(id=seq_along(grid.longitude))

## load species names
spp.names.DF <- read.table('data/Taberlet_etal_EcologyLetters2012/Taberlet_etal_EcologyLetters2012-ReadMe.txt', skip=118, nrows=27, sep='-', stringsAsFactors=FALSE) %>% 
	rename(abbreviation=V1, full=V2) %>%
	mutate(abbreviation=str_trim(abbreviation), full=str_trim(full))
spp.names.DF$binomial <- laply(spp.names.DF[[2]], function(x) {
	return(paste(strsplit(x, '\\ ')[[1]][1:2], collapse='_'))
})

# limit species for analysis if specified
spp.names.DF <- spp.names.DF[all.spp,,drop=FALSE]

## load species AFLP data
spp.PTHS <- 'data/Taberlet_etal_EcologyLetters2012/GeneticData'  %>%
	dir(pattern='^.*\\.csv$', full.names=TRUE, recursive=TRUE)

spp.DF.LST <- spp.PTHS %>%
	llply(fread, data.table=FALSE) %>%
	llply(function(x) {
		# rename duplicate columns
		dups <- which(duplicated(names(x)))
		if (length(dups)==0)
			return(x)
		names(x)[dups] <- paste0(names(x)[dups], '.', seq_along(dups))
		return(x)
	})


# prepare species aflp data
spp.AFLP.LST <- llply(spp.names.DF[[1]], function(x) {
	# init
	curr.pos <- grep(x, spp.PTHS, fixed=TRUE)
	if (length(curr.pos)==1)
		return(spp.DF.LST[[curr.pos]])
	# prelim
	sub.DF.LST <- spp.DF.LST[curr.pos]
	names.LST <- sub.DF.LST %>% llply(names)
	# check that alps and carpathian datasets share a suitable proportion of loci
	intersect.CHR <- intersect(names.LST[[1]], names.LST[[2]])
	prop.loci.shared <- min(((length(intersect.CHR)-3)/(length(names.LST[[1]])-3)),
		((length(intersect.CHR)-3)/(length(names.LST[[2]])-3)))
	if (prop.loci.shared < general.params.LST[[MODE]]$min.loci.shared.proportion)
		return(NULL)
	# return merged table
	return(rbind.fill(sub.DF.LST))
})

# store omitted species
omitted.species.names.DF <- spp.names.DF[sapply(spp.AFLP.LST, is.null),]
spp.names.DF <- spp.names.DF[!sapply(spp.AFLP.LST, is.null),]

# remove omitted species
spp.AFLP.LST <- spp.AFLP.LST[!sapply(spp.AFLP.LST, is.null)]

# construct StructureData objects 
spp.StructureData.LST <- llply(
	spp.AFLP.LST,
	function(x) {
		curr.sub <- select(x, -Cell, -longitude, -latitude)
		return(
				structurer:::StructureData(
				as.matrix(curr.sub),
				names(curr.sub),
				x$Cell
			)
		)
	}
)

## compile species occurence data
# load in data
spp.samples.DF <- ldply(
	seq_along(spp.AFLP.LST),
	.fun=function(i) {
		spp.AFLP.LST[[i]] %>%
			filter(!grepl('B', Cell, fixed=TRUE)) %>% #remove replicate runs for the sample individuals
			mutate(species=spp.names.DF[[3]][i], Cell=gsub('\\-.*$', '', Cell)) %>%
			rename(cell=Cell, sample.longitude=longitude, sample.latitude=latitude) %>%
			select(cell, sample.longitude, sample.latitude, species) %>%
			return()
	}
) %>%
	left_join(grid.DF,by='cell')

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


## save .rda
save.session('results/.cache/01-load-data.rda', compress='xz')
