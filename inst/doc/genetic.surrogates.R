## ---- include=FALSE------------------------------------------------------
# set global cache
knitr::opts_chunk$set(cache=FALSE)

# load packages
library(genetic.surrogates)
library(data.table)
library(rgeos)
library(ggplot2)
library(grid)
library(gridExtra)
library(plyr)
library(dplyr)
library(pander)

# set pander options
panderOptions('knitr.auto.asis', FALSE)

# set default select method
select <- dplyr::select

### set parameters
## debugging parameters
bs.threshold <- 0.5
bs.threads <- 1
bs.n <- 50
bs.thin <- 1
bs.nbp <- 10
bs.pilot <- 50
bs.burn <- 10
mds.k <- 2
mds.trymax <- 2
n.spp <- 3

## analysis parameters
# bs.threshold <- 0.95
# bs.threads <- 10
# bs.n <- 5000
# bs.thin <- 10
# bs.nbp <- 20
# bs.pilot <- 5000
# bs.burn <- 50000
# mds.k <- 2
# mds.trymax <- 100

# set seed for reproducibility
set.seed(500)

## ------------------------------------------------------------------------
## compile spatial grid data
# load grid cell centroids
grid.DF <- fread(
	'extdata/Data_Meirmans_et_al_IntrabioDiv/ReadMe.txt',
	data.table=FALSE,
	skip='cell\tLong\tLat'
) %>% rename(
		grid.longitude=Long,
		grid.latitude=Lat
) %>% mutate(
	id=seq_along(grid.latitude)
)

## ------------------------------------------------------------------------
# load in aflp data
spp.aflp.paths <- dir(
	'extdata/Data_Meirmans_et_al_IntrabioDiv',
	'^.*AFLP\\.dat$',
	full.names=TRUE
)[seq_len(n.spp)]
spp.BayeScanData.LST <- llply(
	spp.aflp.paths,
	read.BayeScanData
)

## ------------------------------------------------------------------------
## compile species occurence data
# load in data
spp.loc.paths <- dir(
	'extdata/Data_Meirmans_et_al_IntrabioDiv',
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

## ------------------------------------------------------------------------
# assign cells as populations
spp.BayeScanData.LST <- llply(
	seq_along(unique(spp.samples.DF$species)),
	function(i) {
		bd <- spp.BayeScanData.LST[[i]]
		bd@populations <- filter(spp.samples.DF, species==unique(spp.samples.DF$species)[i])$cell
		return(bd)
	}
)
# run BayeScan
spp.BayeScan.LST <- llply(
	spp.BayeScanData.LST,
	run.BayeScan,
	threshold=bs.threshold,
	threads=bs.threads,
	n=bs.n,
	thin=bs.thin,
	nbp=bs.nbp,
	pilot=bs.pilot,
	burn=bs.burn
)
# run MDS
spp.mds.LST <- llply(
	spp.BayeScan.LST,
	function(i) {
		`names<-`(llply(c('adaptive', 'neutral'), function(j) {
			if (sum(i@results@fst==j)==0)
				return(NULL)
			return(
				mds(
					i,
					metric='gower',
					type=j,
					k=mds.k,
					trymax=mds.trymax
				)
			)
		}), c('adaptive','neutral'))
	}
)
# store mds rotations for each sample
spp.samples.DF <- ldply(seq_along(unique(spp.samples.DF$species)), .fun=function(i) {
	x <- filter(spp.samples.DF, species==unique(spp.samples.DF$species)[i])
	for (j in c('adaptive', 'neutral')) {
		if (!is.null(spp.mds.LST[[i]][[j]])) {
			x <- cbind(
				x,
				`names<-`(
					as.data.frame(spp.mds.LST[[i]][[j]]$points),
					paste0(j,'_d',seq_len(mds.k))
				)
			)
		}
	}
	return(x)
})
# store mds average rotation for each grid
for (i in seq_along(unique(spp.samples.DF$species))) {
	for (j in c('adaptive', 'neutral')) {
		if(!is.null(spp.mds.LST[[i]][[j]])) {
			curr.sub <- filter(spp.samples.DF, species==unique(spp.samples.DF$species)[i])
			for (k in seq_len(mds.k)) {
				curr.vals <- tapply(
					curr.sub[[paste0(j,'_d',k)]],
					curr.sub$cell,
					FUN=mean
				)
				curr.pos <- match(names(curr.vals), grid.DF$cell)
				grid.DF[curr.pos,paste0(unique(spp.samples.DF$species)[i],'_',j,'_d',k)] <- curr.vals
			}
		}
	}
}

## ------------------------------------------------------------------------
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
bioclim.STK <- stack('extdata/BioClim_variables/bioclim_pca.tif')
# extract mean for each cell for each principle component
extract.DF <- grid.PPLY %>% rasterize(bioclim.STK, field='id') %>% 
	zonal(x=bioclim.STK) %>% as.data.frame() %>% select(-1) %>%
	`names<-`(paste0('env_d',seq_len(nlayers(bioclim.STK))))
# merge with grid.DF
grid.DF <- cbind(grid.DF, extract.DF)
## update spatial objects
grid.PLY@data <- grid.DF
grid.PPLY@data <- grid.DF

## ---- fig.width=9.5, fig.height=10, out.width='7.5in', out.height='8.5in', fig.cap='Species distributions. Squares represent planning units. For a given species, planning units that were found to be inhabited are denoted with bright blue.'----
## plot map of species distributions
# download basemap
data(countriesHigh)
countries.FPLY <- countriesHigh[
	countriesHigh$ADMIN %in% c(
		'Italy', 'Switzerland', 'France', 'Austria',
		'Germany', 'Slovenia', 'Croatia', 'Hungary',
		'Monaco', 'Germany'
	)
,] %>% spFortify
# fortify data
grid.FPLY <- spFortify(grid.PLY)
spp.grid.FPLY <- ldply(unique(spp.samples.DF$species), function(x) {
		z <- grid.FPLY[,c('long', 'lat', 'group', x),drop=FALSE]
		names(z)[4] <- 'presence'
		z$species <- gsub('\\_', ' ', x)
		return(z)
	}
)
# plot species data
ggplot() +
	geom_polygon(data=countries.FPLY, aes(x=long, y=lat, group=group),
		fill='grey20', color='grey80') +
	geom_polygon(data=spp.grid.FPLY, aes(x=long, y=lat, 
		group=group, fill=presence), alpha=0.8, color='grey10') +
	theme_classic() +
	guides(fill=guide_legend(title='Presence')) +
	theme(axis.ticks=element_blank(), axis.text=element_blank()) +
	coord_cartesian(
		xlim=buffered.range(grid.FPLY$long, 0.05),
		ylim=buffered.range(grid.FPLY$lat, 0.05)
	) +
	xlab('') +
	ylab('') +
	facet_wrap(~ species, ncol=4)

## ---- fig.width=4, fig.height=3.5, fig.cap="Species richness. Squares denote planning units. Planning units with a brighter color are inhabited by more species."----
# calculate species richness
grid.PLY$Species_richness <- grid.PLY@data %>%
	select(5:(4+n.spp)) %>% as.matrix() %>% rowSums()

# plot species richness
ggplot() +
	geom_polygon(data=countries.FPLY, aes(x=long, y=lat, group=group),
		fill='grey20', color='grey80') +
	geom_polygon(data=spFortify(grid.PLY), aes(x=long, y=lat, 
		group=group, fill=Species_richness), alpha=0.8, color='grey10') +
	guides(fill=guide_legend(title='Count (#)')) +
	theme_classic() +
	theme(axis.ticks=element_blank(), axis.text=element_blank()) +
	coord_cartesian(
		xlim=buffered.range(grid.FPLY$long, 0.05),
		ylim=buffered.range(grid.FPLY$lat, 0.05)
	) +
	xlab('') +
	ylab('') +
	ggtitle('Species richness')

## ------------------------------------------------------------------------
knitr::kable(
	ldply(
		seq_along(unique(spp.samples.DF$species)), 
		function(i) {
			ldply(
				seq_along(spp.mds.LST[[i]]),
				function(j) {
				data.frame(
					Species=paste0('\\textit{',gsub('\\_', ' ', unique(spp.samples.DF$species)[i]),'}'),
					Loci=names(spp.mds.LST[[i]])[j],
					Stress=spp.mds.LST[[i]][[j]]$stress,
					Converged=spp.mds.LST[[i]][[j]]$converged
				)
			})
		}
	),
	digits=2,
	caption='Summary of nonmetric-dimensional scaling (MDS) analyses on genetic variation for each species.'
)

## ------------------------------------------------------------------------
plot.spp.mds <- function(i) {
	do.call(
		grid.arrange,
		append(
			unlist(llply(c('adaptive','neutral'), function(g) {
				llply(seq_len(mds.k), function(k) {
					ggplot() +
						geom_polygon(data=countries.FPLY, aes(x=long, y=lat, group=group),
							fill='grey20', color='grey80') +
						geom_polygon(data=grid.FPLY, aes_string(x='long', y='lat', 
							group='group', fill=paste0(unique(spp.samples.DF$species)[i], '_', j, '_d',k)),
							alpha=0.8, color='grey10') +
						guides(fill=guide_legend(title=' ')) +
						theme_classic() +
						theme(axis.ticks=element_blank(), axis.text=element_blank(),
							plot.margin=unit(c(0,0,0,0),'cm')) +
						coord_cartesian(
							xlim=buffered.range(grid.FPLY$long, 0.05),
							ylim=buffered.range(grid.FPLY$lat, 0.05)
						) +
						xlab('') +
						ylab('') +
						ggtitle(paste0(g,' (',k,')'))
				})
			}),recursive=FALSE),
			list(ncol=2)
		)
	)
}

## ---- fig.width=6.5, fig.height=4.5, fig.cap=paste0('Distribution of adaptive and neutral genetic variation in \\textit{',gsub('\\_', ' ', unique(spp.samples.DF$species)[1]),'}. Each square represents a planning unit. The color of each planning unit panel corresponds to ordination values. Planning units with similar colors contain individiduals with similar genetic variation.')----
plot.spp.mds(1)

## ---- fig.width=6.5, fig.height=4.5, fig.cap=paste0('Distribution of adaptive and neutral genetic variation in \\textit{',gsub('\\_', ' ', unique(spp.samples.DF$species)[2]),'}. See Figure XX caption for conventions.')----
plot.spp.mds(2)

## ---- fig.width=6.5, fig.height=4.5, fig.cap=paste0('Distribution of adaptive and neutral genetic variation in \\textit{',gsub('\\_', ' ', unique(spp.samples.DF$species)[3]),'}. See Figure XX caption for conventions.')----
plot.spp.mds(3)

## ------------------------------------------------------------------------
## load pca summary
pca.DF <- read.table('extdata/BioClim_variables/pca.TXT', skip=80) %>% `names<-`(
	c('Principle Component', 'Eigen Value', 'Variation explained (%)',
	'Accumulative variation explained (%)')
)
## make results table showing Eigen values
knitr::kable(
	pca.DF,
	digits=2,
	caption='Summary of priciniple components analysis (PCA) on bioclimatic variation across the study area. The first two principle components (PCs) were used for subsequent analysis.'
)

## ---- width=6.5, height=4.5, fig.cap='Climatic variation. Each panel depicts variation based on a different principle component (PC). Sqaures represent planning units. The color of each planning unit denotes the average priciniple component value of pixels inside it. Planning units with more similar colors have more similar climates regimes.'----
do.call(
	grid.arrange,
		append(
		llply(grep('^env\\_.*$', names(grid.DF), value=TRUE), function(x) {
			ggplot() +
				geom_polygon(data=countries.FPLY, aes(x=long, y=lat, group=group),
					fill='grey20', color='grey80') +
				geom_polygon(data=grid.FPLY, aes_string(x='long', y='lat', 
					group='group', fill=x),
					alpha=0.8, color='grey10') +
				guides(fill=guide_legend(title=' ')) +
				theme_classic() +
				theme(axis.ticks=element_blank(), axis.text=element_blank(),
					plot.margin=unit(c(0,0,0,0),'cm')) +
				coord_cartesian(
					xlim=buffered.range(grid.FPLY$long, 0.05),
					ylim=buffered.range(grid.FPLY$lat, 0.05)
				) +
				xlab('') +
				ylab('') +
				ggtitle(paste0('PC ', substr(x, nchar(x), nchar(x))))
		}),
		list(ncol=2)
	)
)

