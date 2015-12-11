#' @include dependencies.R genetic.surrogates-internal.R

#' Buffered range 
#'
#' This function returns the range of a vector that has been buffered by a percentage.
#' 
#' @param x \code{numeric} vector.
#' @return \code{numeric} vector of two elements.
#' @seealso \code{\link[base]{range}}.
#' @examples
#' buffered.range(c(0,0.5,1), 0.05)
#' @export
buffered.range <- function(x, percent=0.05) {
	range(x)+(c(-1, 1) * ((diff(range(x))*percent)))
}

#' spFortify
#' 
#' This function is a convienience function designed to prepare a \code{Spatial*DataFrame} object
#' for visualisation using ggplot2.
#' @param x \code{\link[sp]{SpatialLinesDataFrame}}, \code{\link[sp]{SpatialPointsDataFrame}}, or \code{\link[sp]{SpatialPolygonsDataFrame}} object.
#' @param columns \code{character} with column names to include in returned object.
#' @return \code{data.frame}
#' @seealso \code{\link[ggplot2]{fortify}}, \code{\link[sp]{SpatialLinesDataFrame}}, \code{\link[sp]{SpatialPointsDataFrame}} \code{\link[sp]{SpatialPolygonsDataFrame}}.
#' @export
spFortify <- function(x, columns=names(x@data)) {
	if ('id' %in% names(x))
		x@data <- rename(x@data, id2=id)
	x$fid <- seq_len(nrow(x@data))
	f <- fortify(x, region="fid")
	f <- merge(f, x@data, by.x='id', by.y='fid')
	f <- f[,which(names(f) != 'fid')]
	return(f)
}

