#' Make target data using attribute spaces
#'
#' This function takes a set of species names and 
#' attribute spaces and generates a table with targets.
#'
#' @param species \code{character} speces names.
#' @param environmental.space \code{AttributeSpace} object with environmental space data.
#' @param geographic.space \code{AttributeSpace} object with geographic space data.
#' @param adaptive.spaces \code{list} of \code{AttributeSpace} objects with adaptive space data for species with loci undergoing adaptation.
#' @param neutral.spaces \code{list} of \code{AttributeSpace} objects with netrual space data for species with loci not undergoing adaptation. 
#' @details The \code{AttributeSpace} objects in the arguments to \code{adaptive.spaces} and \code{neutral.spaces} are special, such that the species they are not associated with will have negative weights.
#' @seealso \code{\link[rapr]{AttributeSpace}}.
#' @export
make.targets <- function(
	species, 
	environmental.space, geographic.space, 
	adaptive.spaces, neutral.spaces,
	amount.target=0.2, space.target=0.2
) {
	# amount targets
	amount.targets.DF <- data.frame(
		species=seq_along(species),
		target=rep(0, length(species)),
		proportion=rep(amount.target, length(species))
	)
	row.names(amount.targets.DF) <- paste0('amount_',species)
	# surrogate.spaces
	surrogate.targets.DF <- data.frame(
		species=rep(seq_along(species), 2),
		target=c(rep(1, length(species)), rep(2, length(species))),
		proportion=rep(space.target, length(species)*2)
	)
	row.names(surrogate.targets.DF) <- c(paste0('env_',species), paste0('geo_',species))
	# adaptive spaces
	adaptive.targets.DF <- ldply(seq_along(adaptive.spaces), .fun=function(i) {
		# get species position
		spp.pos <- which(sapply(adaptive.spaces[[i]]@demand.points, function(y) {y@weights[1]})>0)
		# return data.frame
		spp.DF <- data.frame(
			species=seq_along(species),
			target=rep(i+2, length(species)),
			proportion=replace(
				rep(0, length(species)),
				spp.pos,
				space.target
			),
			name=replace(
				paste0('nulla_',i,'_',species),
				spp.pos,
				paste0('adaptive_',species[spp.pos])
			)
		)
		return(spp.DF)
	})
	# neutral
	neutral.targets.DF <- ldply(seq_along(neutral.spaces), .fun=function(i) {
		# get species position
		spp.pos <- which(sapply(neutral.spaces[[i]]@demand.points, function(y) {y@weights[1]})>0)
		# return data.frame
		spp.DF <- data.frame(
			species=seq_along(species),
			target=rep(i+2, length(species)),
			proportion=replace(
				rep(0, length(species)),
				spp.pos,
				space.target
			),
			name=replace(
				paste0('nulln_',i,'_',species),
				spp.pos,
				paste0('neutral_',species[spp.pos])
			)
		)
		return(spp.DF)
	})
	# assign rownames
	row.names(adaptive.targets.DF) <- as.character(adaptive.targets.DF$name)
	row.names(neutral.targets.DF) <- as.character(neutral.targets.DF$name)
	# return
	mutate(do.call(rbind, list(
		amount.targets.DF, surrogate.targets.DF,
		select(adaptive.targets.DF, -name), select(neutral.targets.DF, -name)
	)), target=as.integer(target), species=as.integer(species))
}