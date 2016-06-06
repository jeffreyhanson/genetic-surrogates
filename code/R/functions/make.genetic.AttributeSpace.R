#' Make AttributeSpace using genetic data
#'
#' This function creates an \code{AttributeSpace} object with for a single species. 
#'
#' @param site.data \code{data.frame} with coordinates for planning units.
#' @param species.data \code{data.frame} with coordinates of demand points for the species.
#' @return \code{AttributeSpace}.
#' @seealso \code{\link[rapr]{AttributeSpace}}, \code{\link[rapr]{AttributeSpace}}.
#' @export
make.genetic.AttributeSpace <- function(site.data,species.data,species) {
	# extract ids of pus where the species is found
	curr.ids<-which(rowSums(is.na(site.data))==0)
	# return attribute space
	return(
		AttributeSpace(
			planning.unit.points=PlanningUnitPoints(
				coords=as.matrix(site.data[curr.ids,,drop=FALSE]),
				ids=curr.ids
			),
			demand.points=DemandPoints(
				coords=as.matrix(species.data),
				weights=rep(1, nrow(species.data))
			),
			species=as.integer(species)
		)
	)
}
