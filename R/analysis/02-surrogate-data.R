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

## extract geographic data
centroids.DF <- gCentroid(grid.PPLY, byid=TRUE) %>% slot('coords') %>%
	as.data.frame() %>% `names<-`(paste0('geo_d',1:2))
grid.DF <- cbind(grid.DF, centroids.DF)

## extract climatic data
# load climatic data
bioclim.STK <- stack('data/BioClim_variables/bioclim_pca.tif')
# extract mean for each cell for each principle component
extract.DF <- grid.PPLY %>% rasterize(bioclim.STK, field='id') %>% 
	zonal(x=bioclim.STK) %>% as.data.frame() %>% select(-1) %>%
	`names<-`(paste0('env_d',seq_len(nlayers(bioclim.STK))))
# merge with grid.DF
grid.DF <- cbind(grid.DF, extract.DF)

## update spatial objects
grid.PLY@data <- grid.DF
grid.PPLY@data <- grid.DF
 
## load and save pca summary
pca.DF <- read.table('data/BioClim_variables/pca.TXT', skip=80) %>% `names<-`(
	c('Principle Component', 'Eigen Value', 'Variation explained (%)',
	'Accumulative variation explained (%)')
)

