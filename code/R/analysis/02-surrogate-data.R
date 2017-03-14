## load .rda
checkpoint::checkpoint('2016-11-26', R.version='3.3.2', scanForPackages=FALSE)
session::restore.session('data/intermediate/01-load-data.rda')

## load parameters
surrogate.params.LST <- parseTOML('code/parameters/surrogate.toml')

## create spatial data
# grid data as SpatialPolygonsDataFrame
grid.PTS <- SpatialPoints(as.matrix(grid.DF[,2:3]))
grid.PLY <- grid.PTS %>%
	points2grid(tolerance=0.05) %>%
	as('SpatialPolygons')
grid.PLY <- grid.PLY[sapply(gIntersects(grid.PTS, grid.PLY, byid=TRUE, returnDense=FALSE), `[[`, 1),] %>%
	spChFIDs(
		as.character(seq_len(nrow(grid.DF)))
	) %>% 
	SpatialPolygonsDataFrame(
		data=grid.DF
	)
grid.PLY@proj4string <- wgs1984
grid.PPLY <- spTransform(grid.PLY, europeEA)
# sample data as SpatialPoints
spp.sample.PTS <- SpatialPointsDataFrame(
	coords=as.matrix(spp.samples.DF[,5:6]),
	data=spp.samples.DF,
	proj4string=wgs1984
)
spp.sample.PPTS <- spTransform(spp.sample.PTS, europeEA)

## geographic data
# extract it
centroids.DF <- grid.PPLY %>%
	spTransform(europeED) %>%
	gCentroid(byid=TRUE) %>%
	slot('coords') %>%
	as.data.frame() %>%
	`names<-`(paste0('geo_d',1:2))
# zscore it
centroids.DF <- sweep(centroids.DF, 2, MARGIN=2, FUN='-', colMeans(centroids.DF))
centroids.DF <- sweep(centroids.DF, 2, MARGIN=2, FUN='/', apply(centroids.DF, 2, sd))
# append to main data.frame
grid.DF <- cbind(grid.DF, centroids.DF)

## remove grids without any species occurences
# idenitfy pus with occurences
valid.rows <- which(apply(grid.DF[,unique(spp.samples.DF$species),drop=FALSE], 1, sum)>0)
# subset objects
grid.DF <- grid.DF[valid.rows,,drop=FALSE]
grid.PLY <- grid.PLY[valid.rows,]
grid.PPLY <- grid.PPLY[valid.rows,]
centroids.DF <- centroids.DF[valid.rows,,drop=FALSE]
# reset ids
grid.DF$id <- seq_len(nrow(grid.DF))
grid.PLY$id <- seq_len(nrow(grid.DF))
grid.PPLY$id <- seq_len(nrow(grid.DF))

## extract climatic data
# load climatic data
bioclim.STK <- stack(dir('data/raw/BioClim_variables', '^.*\\.tif$', full.names=TRUE))
studyarea.PLY <- grid.PPLY %>%
	extent() %>%
	as('SpatialPolygons') %>%
	gBuffer(width=20000, byid=FALSE)  %>%
	`slot<-`(name='proj4string', value=grid.PPLY@proj4string) %>%
	spTransform(bioclim.STK@crs)
bioclim.STK <- bioclim.STK %>% crop(studyarea.PLY) %>% mask(studyarea.PLY) %>% projectRaster(crs=grid.PPLY@proj4string, res=1000)
# extract mean for each cell for each principle component
extract.DF <- grid.PPLY %>% rasterize(bioclim.STK, field='id') %>% 
	zonal(x=bioclim.STK, fun='mean') %>%  as.data.frame() %>% select(-1) %>%
	`names<-`(paste0('bio_d',seq_len(nlayers(bioclim.STK))))
# merge with grid.DF
grid.DF <- cbind(grid.DF, extract.DF)
# store pca roations for each planning unit
spp.pca.DF <- data.frame(species=character(0), percent_variation_explained=numeric(0))
for (i in seq_along(unique(spp.samples.DF$species))) {
	# extract bio vars for species
	curr.sub.DF <- filter(spp.samples.DF, species==unique(spp.samples.DF$species)[i])
	curr.grids.DF <- grid.DF %>%
		filter(cell %in% curr.sub.DF[['cell']]) %>%
		select(cell, starts_with('bio_d'))
	# run pca
	curr.PCA <- prcomp(~ . -cell, curr.grids.DF, center=TRUE, scale=TRUE)
	# store pca results
	spp.pca.DF <- spp.pca.DF %>% 
		rbind(data.frame(species=unique(spp.samples.DF$species)[i], percent_variation_explained=summary(curr.PCA)$importance[3,surrogate.params.LST[[MODE]]$number.components]))
	# store pca rotations
	grid.DF <- left_join(
		grid.DF,
		curr.PCA$x[,seq_len(surrogate.params.LST[[MODE]]$number.components)] %>%
			data.frame() %>%
			`names<-`(paste0(unique(spp.samples.DF$species)[i], '_env_d',seq_len(surrogate.params.LST[[MODE]]$number.components))) %>%
			mutate(cell=curr.grids.DF$cell),
		by='cell'
	)
}

## extract population density data
# load pop data
pop.RST <- raster('data/raw/GRUMP_V1_Population_Density/grumpv1-popdensity.tif') %>% 
	crop(grid.PLY) %>% projectRaster(crs=grid.PPLY@proj4string)
# extract total for each cell
extract2.DF <- grid.PPLY %>% rasterize(pop.RST, field='id') %>% 
	zonal(x=pop.RST, fun='sum') %>% as.data.frame() %>%
	select(-1) %>% `names<-`('pop.density') %>% mutate(pop.density=pop.density/1000)
# merge with grid.DF
grid.DF <- cbind(grid.DF, extract2.DF)
	
## update @data slot in spatial objects
grid.PLY@data <- grid.DF
grid.PPLY@data <- grid.DF

## save .rda
save.session('data/intermediate/02-surrogate-data.rda', compress='xz')
