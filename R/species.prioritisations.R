#' Generate prioritisation using specific targets
#'
#' This function is a wrapper for updating targets in a \code{RapUnsolved} object and solving it.
#'
#' @param x \code{RapUnsolved} object.
#' @param amount.targets \code{numeric} amount-based targets to use.
#' @param surrogate.targets \code{numeric} surrogate-based targets to use.
#' @param genetic.targets \code{numeric} genetic-based targets to use.
#' @param ... arguments passed to solve.
#' @return \code{RapSolved}
species.prioritisation <- function(x, amount.targets, surrogate.targets, genetic.targets, ...) {
	# init
	surrogate.pos <- c(grep('^env\\_.*$', row.names(x@data@targets)), grep('^geo\\_.*$', row.names(x@data@targets)))
	genetic.pos <- c(grep('^neutral\\_.*$', row.names(x@data@targets)), grep('^adaptive\\_.*$', row.names(x@data@targets)))
	# update targets
	x@data@targets[which(row.names(x@data@targets)==0),'proportion'] <- amount.targets
	x@data@targets[surrogate.pos,'proportion'] <- surrogate.targets
	x@data@targets[genetic.pos,'proportion'] <- genetic.targets
	# solve object
	return(solve(x, ...))
}

