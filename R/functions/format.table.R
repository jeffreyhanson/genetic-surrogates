#' Format table for knitr
#'
#' This function formats tables for rendering in scientific reports.
#'
#' @param x \code{data.frame}.
#' @param omit \code{character} name of columns to omit from processig.
#' @return \code{data.frame}
format.table <- function(x, omit=c()) {
	# replace consecutive duplicate factors with ''
	for (i in seq_len(ncol(x))) {
		if (inherits(x[[i]], c('factor', 'character')) & (!names(x)[i] %in% omit)) {
			x[[i]] <- as.character(x[[i]])
			curr.value <- character(1)
			for (j in seq_len(nrow(x))) {
				if (x[j,i]==curr.value) {
					x[j,i] <- ''
				} else {
					curr.value <- x[j,i]
				}
			}
		}
	}
	# return data.frame
	return(x)
}
