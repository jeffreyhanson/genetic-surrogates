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

#' pretty.pval
#' 
#' This function returns the rounded version of a p-value for use in text along with the correct arithmatic operator.
#' @param x \code{numeric} p-value
#' @return \code{character} object.
#' @example
#' pretty.pval(0.04)
#' pretty.pval(0.0000001)
#' pretty.pval(0.565656)
#' @export
pretty.pval <- function(x) {
	if (x > 1)
		return('> 1')
	if (x > 0.1)
		return('> 0.1')
	if (x > 0.05)
		return('> 0.05')
	if (x < 0.001)
		return('< 0.001')
	if (x < 0.01)
		return('< 0.01')
	if (x < 0.05)
		return('< 0.05')
}

#' pretty.tval
#' 
#' This function returns a floored version of a test statistic for use in text along with the correct arithmatic operator.
#' @param x \code{numeric} test statistic or multiple test statistics.
#' @return \code{character} object.
#' @example
#' pretty.tval(0.04)
#' pretty.tval(c(0.01, 3, 0.5))
#' @export
pretty.tval <- function(x, digits=1) {
	x <- x*(10^digits)
	x <- floor(x)
	x <- x/(10^digits)
	return(x)
}


#' extract.GLHT
#'
#' Extract maximum p-value from multiple comparisons object given a filter.
#' @param x \code{glht} object.
#' @param filter1 \code{character} first filter expression 
#' @param filter2 \code{character} second filter expression 
#' @export
extract.GLHT <- function(x, filter1, filter2) {
	rownames <- names(x$test$tstat)
	first.pos <- grep(filter1, rownames)
	second.pos <- grep(filter2, rownames)
	return(
		list(
			t=pretty.tval(max(unname(x$test$tstat))),
			p=pretty.pval(max(unname(x$test$pvalues)))
		)
	)
}

#' Convert lower triangle matrix of p-values to full matrix (V 1.02, 2015 04 23)
#' @author Sal Mangiafico, Rutgers Cooperative Extension (http://rcompanion.org/)
#' @seealso \url{http://rcompanion.org/rcompanion/d_06.html}
#' @example
#' PT = full.p.table(pairwise.wilcox.test(x, g, p.adjust.method=\"none\")$p.value)
full.p.table = function(PT) {
	PTa <- rep(c(NA_real_),length(PT[,1]))         # Add a row
	PT1 <- rbind(PTa, PT)
	rownames(PT1)[1] <- colnames(PT1)[1]
	PTb <- rep(c(NA_real_),length(PT1[,1]))        # Add a column
	PT1 <- cbind(PT1, PTb)
	n <- length(PT1[,1])
	colnames(PT1)[n] <- rownames(PT1)[n]
	PT2 = t(PT1)                                   # Create new transposed
	PT2[lower.tri(PT2)] = PT1[lower.tri(PT1)]
	diag(PT2) = signif(1.00, digits = 4)           # Set the diagonal
	return(PT2)
}

