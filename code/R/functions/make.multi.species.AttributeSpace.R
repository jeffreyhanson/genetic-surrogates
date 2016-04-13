#' Make multi-species AttributeSpace object
#'
#' This function generates an \code{AttributeSpace} object using 
#' values in a \code{data.frame} and occupancy data in a second 
#' \code{data.frame}.
#' @param site.data \code{data.frame} with values for planning units in each attribute space.
#' @param species.data \code{data.frame} with binary occupancy data. Each column is for a differnet species. Each row is for a different planning unit.
#' @param distance.metric \code{character} name of distance metric to use for attribute space
#' @details The demand points for each species are generated as the coordinates of the planning units
#' that the species is found in.
#' @return \code{AttributeSpace}
#' @seealso \code{\link[rapr]{AttributeSpace}}, \code{\link[rapr]{DemandPoints}}.
#' @export
make.multi.species.AttributeSpace <- function(site.data, species.data, distance.metric='euclidean') {
	return(
		AttributeSpace(
			pu=SimplePoints(as.matrix(site.data)),
			demand.points=alply(species.data, 2, .fun=function(x) {
				return(DemandPoints(
					points=SimplePoints(as.matrix(site.data[x==1,,drop=FALSE])),
					weights=rep(1, sum(x))
				))
			}),
			distance.metric=distance.metric
		)
	)
}

