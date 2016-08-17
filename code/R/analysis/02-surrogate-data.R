## load .rda
checkpoint::checkpoint('2016-08-03', R.version='3.3.0', scanForPackages=FALSE)
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
bioclim.STK <- stack('data/raw/BioClim_variables/bioclim_pca.tif')
bioclim.STK <- bioclim.STK[[seq_len(surrogate.params.LST[[MODE]]$number.components)]]
# zscore layers --note that they already have unit sd
bioclim.means <- cellStats(bioclim.STK, 'mean')
bioclim.STK <- (bioclim.STK - bioclim.means)
# extract mean for each cell for each principle component
extract.DF <- grid.PPLY %>% rasterize(bioclim.STK, field='id') %>% 
	zonal(x=bioclim.STK) %>%  as.data.frame() %>% select(-1) %>%
	`names<-`(paste0('env_d',seq_len(nlayers(bioclim.STK))))
# merge with grid.DF
grid.DF <- cbind(grid.DF, extract.DF)

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

## load and save pca summary
pca.DF <- read.table('data/raw/BioClim_variables/bioclim_pca.TXT', skip=94) %>% `names<-`(
	c('Principle Component', 'Eigen Value', 'Variation explained (%)',
	'Accumulative variation explained (%)')
)

## save .rda
save.session('data/intermediate/02-surrogate-data.rda', compress='xz')
