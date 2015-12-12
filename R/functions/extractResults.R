#' Extract results from RapSolved object
#'
#' This function extracts results from a RapSolved object
#'
#' @param x \code{RapSolved} object.
#' @export
extractResults <- function(x) {
	# init
	extract.held <- function(x, spp.name, space.name) {
		pos <- which(x@data@targets$name==paste0(space.name,'_',spp.name))
		space.index <- x@data@targets$target[pos]
		spp.index <- x@data@targets$species[pos]
		return(c(space.held(x, species=spp.index, space=space.index)))
	}
	# extract amount held
	amount.held.DF <- data.frame(
		Species=x@data@species$name,
		amount.held=c(amount.held(x))
	)
	# extract space held
	space.held.MTX <- space.held(x)
	space.held.DF <- ldply(x@data@species$name, function(y) {
		# init
		adaptive.held <- extract.held(x, y, 'adaptive')
		if (length(adaptive.held)==0) adaptive.held <- NA_real_
		neutral.held <- extract.held(x, y, 'neutral')
		if (length(neutral.held)==0) neutral.held <- NA_real_
		# make data.frame
		data.frame(
			environmental.held = extract.held(x, y, 'env'),
			geographic.held = extract.held(x, y, 'geo'),
			adaptive.held = adaptive.held,
			neutral.held = neutral.held
		)
	})
	# return data.frame
	return(
		cbind(
			amount.held.DF,
			space.held.DF
		)
	)
}