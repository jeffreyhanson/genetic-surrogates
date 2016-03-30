#' Compact letter display
#'
#' This function takes a post-hoc analysis indicating differences between different groups and returns
#' Ia vector of letters indicating which groups are different from each other.
#' For instance, outputs from the \code{\link[RVAideMemoire]{fisher.multcomp}}
#' function are a valid input. 
#' @param x \code{RV.multcomp} object.
#' @return \code{character} vector.
#' @seealso \code{\link[multcomp]{cld}}
cld.RV.multcomp <- function(x) {
	multcompView::multcompLetters(
		full.p.table(x$p.value),
		compare="<",
		threshold=0.05,
		Letters=LETTERS,
		reversed=FALSE
	)
}

