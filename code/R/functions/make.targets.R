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
#' @seealso \code{\link[raptr]{AttributeSpace}}.
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
		proportion=rep(amount.target, length(species)),
		name=paste0('amount_',species)
	)
	# surrogate.spaces
	surrogate.targets.DF <- data.frame(
		species=rep(seq_along(species), 2),
		target=c(rep(1, length(species)), rep(2, length(species))),
		proportion=rep(space.target, length(species)*2),
		name=c(paste0('env_',species), paste0('geo_',species))
	)
	# adaptive spaces
	adaptive.targets.DF <- ldply(seq_along(adaptive.spaces@spaces), .fun=function(i) {
		# return data.frame
		return(
			data.frame(
				species=adaptive.spaces@spaces[[i]]@species,
				target=3,
				proportion=space.target,
				name=paste0('adaptive_',species[adaptive.spaces@spaces[[i]]@species])
			)
		)
	})
	# neutral spaces
	neutral.targets.DF <- ldply(seq_along(neutral.spaces@spaces), .fun=function(i) {
		# return data.frame
		return(
			data.frame(
				species=neutral.spaces@spaces[[i]]@species,
				target=4,
				proportion=space.target,
				name=paste0('neutral_',species[neutral.spaces@spaces[[i]]@species])
			)
		)
	})
	# return
	return(mutate(do.call(rbind, list(
		amount.targets.DF, surrogate.targets.DF,
		adaptive.targets.DF, neutral.targets.DF)
	), target=as.integer(target), species=as.integer(species)))
}