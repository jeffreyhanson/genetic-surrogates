#' MLPE model
#'
#' This function runs a maximum likelihood population effects mixed effects model given two pairwise distance matrices.
#' The code for this function is based on \code{\link[ResistanceGA]{MLPE.lmm}}.
#' @param y \code{matrix} object with pairwise genetic distances
#' @param x \code{matrix} object with pairwise surrogate distances
#' @param REML \code{logical} should model be fitted using REML?
#' @example
#' x <- as.matrix(dist(matrix(rnorm(10),ncol=2)))
#' y <- as.matrix(dist(matrix(rnorm(10),ncol=2)))
#' model <- MLPE(y,x,REML=FALSE)
#' print(model)
#' @export
MLPE <- function(y,x,REML=TRUE) {
	# init
	if (inherits(y, 'dist')) y <- as.matrix(y)
	if (inherits(x, 'dist')) x <- as.matrix(x)
	ID <- ResistanceGA:::To.From.ID(POPS = nrow(x))
	ZZ <- ResistanceGA:::ZZ.mat(ID = ID)
	y <- ResistanceGA:::lower(y)
	x <- ResistanceGA:::lower(x)
	# prepare data
	dat <- data.frame(ID, x = x, y = y)
	dat <- dplyr::mutate(dat, x = (mean(x)-x)/sd(x), y = (mean(y)-y)/sd(y))
	colnames(dat) <- c("id1", "id2", "x", "y")
	# prepare model
	mod_list <- list('full'=lme4:::lFormula(y ~ x + (1 | id1), data = dat, REML = REML),
		"null"=lme4:::lFormula(y ~ 1 + (1 | id1), data = dat, REML = REML))
	MODs <- lapply(mod_list, function(x) {
		x$reTrms$Zt <- ZZ
		# fit model
		dfun <- do.call(lme4:::mkLmerDevfun, x)
		opt <- lme4:::optimizeLmer(dfun)
		MOD <- (lme4:::mkMerMod(environment(dfun), opt, x$reTrms, fr = x$fr))
	})
	# return model
	return(MODs)
}

#' Calculate R^2 model from 

