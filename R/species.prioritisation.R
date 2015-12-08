#' Generate prioritisation using specific targets
#'
#' This function is a wrapper for updating targets in a \code{RapUnsolved} object and solving it.
#'
#' @param x \code{RapUnsolved} object.
#' @param amount.targets \code{numeric} amount-based targets to use.
#' @param env.surrogate.targets \code{numeric} environmental surrogate targets.
#' @param geo.surrogate.targets \code{numeric} geographic surrogate targets to use.
#' @param adaptive.genetic.targets \code{numeric} adaptive genetic targets.
#' @param neutral.genetic.targets \code{numeric} neutral genetic targets.
#' @param ... arguments passed to solve.
#' @return \code{RapSolved}
species.prioritisation <- function(x, amount.targets, env.surrogate.targets, geo.surrogate.targets,
	adaptive.genetic.targets, neutral.genetic.targets, ...) {
	# init
	env.surrogate.pos <- grep('^env\\_.*$', x@data@targets$name)
	geo.surrogate.pos <- grep('^geo\\_.*$', x@data@targets$name)
	adaptive.genetic.pos <- grep('^adaptive\\_.*$', x@data@targets$name)
	neutral.genetic.pos <- grep('^neutral\\_.*$', x@data@targets$name)
	# update targets
	x@data@targets[which(x@data@targets$target==0),'proportion'] <- amount.targets
	x@data@targets[env.surrogate.pos,'proportion'] <- env.surrogate.targets
	x@data@targets[geo.surrogate.pos,'proportion'] <- geo.surrogate.targets
	x@data@targets[adaptive.genetic.pos,'proportion'] <- adaptive.genetic.targets
	x@data@targets[neutral.genetic.pos,'proportion'] <- neutral.genetic.targets
	# solve object
	ret<-solve(x, ...)
	return(ret)
}

