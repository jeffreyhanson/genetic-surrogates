#' Make AttributeSpace using data for one species
#'
#' This function creates an \code{AttributeSpace} object with multiple speciess. It
#' has a valid \code{DemandPoints} object for a single species,
#' and has dummy \code{DemandPoints} objects for the remaining species.
#'
#' @param site.data \code{data.frame} with coordinates for planning units.
#' @param species.data \code{data.frame} with coordinates of demand points for the species.
#' @param n.species \code{numeric} number of species to output.
#' @param spp.pos \code{numeric} index of this species.
#' @param distance.metric \code{character} name of distance metric to use for attribute space.
#' @return \code{AttributeSpace}.
#' @seealso \code{\link[rapr]{AttributeSpace}}, \code{\link[rapr]{AttributeSpace}}.
#' @export
make.single.species.AttributeSpace <- function(site.data,species.data,n.species,spp.pos, distance.metric='euclidean') {
	# replace NA values in site.data with -9999
	site.data <- apply(site.data, 2, function(x) {
		replace(x, which(is.na(x)), -9999)
	}) %>% as.data.frame()
	return(
		AttributeSpace(
			pu=SimplePoints(as.matrix(site.data)),
			demand.points=replace(
				replicate(
					n.species,
					DemandPoints(
						points=SimplePoints(coords=matrix(rep(c(-9999, -9998), each=ncol(site.data)), byrow=TRUE, ncol=ncol(site.data))),
						weights=c(1, 1)
					)
				),
				spp.pos,
				DemandPoints(
					points=SimplePoints(coords=as.matrix(species.data)),
					weights=rep(1, nrow(species.data))
				)
			),
			distance.metric=distance.metric
		)
	)
}
