#' Make AttributeSpaces object using surrogate data
#'
#' This function generates an \code{AttributeSpaces} object using 
#' values in a \code{data.frame} and occupancy data in a second 
#' \code{data.frame}.
#' @param site.data \code{data.frame} with values for planning units in each attribute space.
#' @param species.data \code{data.frame} with binary occupancy data. Each column is for a differnet species. Each row is for a different planning unit.
#' @param distance.metric \code{character} name of distance metric to use for attribute space
#' @details The demand points for each species are generated as the coordinates of the planning units
#' that the species is found in.
#' @return \code{AttributeSpace}
#' @seealso \code{\link[raptr]{AttributeSpace}}, \code{\link[raptr]{DemandPoints}}.
#' @export
make.surrogate.AttributeSpaces <- function(site.data, species.data, name) {
	return(
		AttributeSpaces(
			spaces=llply(
				seq_len(ncol(species.data)), 
				.fun=function(i) {
					# set ids 
					curr.ids <- which(species.data[[i]]==1)
					# create as
					AttributeSpace(
						planning.unit.points=PlanningUnitPoints(
							coords=as.matrix(site.data[curr.ids,,drop=FALSE]),
							ids=curr.ids
						),
						demand.points=DemandPoints(
							coords=as.matrix(site.data[curr.ids,,drop=FALSE]),
							weights=rep(1, length(curr.ids))
						),
						species=i
					)
				}
			),
			name=name
		)
	)
}

