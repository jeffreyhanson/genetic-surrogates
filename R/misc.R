#' @include genetic.surrogates-internal.R

#' Buffered range 
#'
#' This function returns the range of a vector that has been buffered by a percentage.
#' 
#' @param x \code{numeric} vector.
#' @return \code{numeric} vector of two elements.
#' @seealso \code{\link[base]{range}}
#' @export
#' @examples
#' buffered.range(c(0,0.5,1), 0.05)
buffered.range <- function(x, percent=0.05) {
	range(x)+(c(-1, 1) * ((diff(range(x))*percent)))
}
