#' @include genetic.surrogates-internal.R misc.R

#' BayeScanResults: An S4 class to results from BayeScan
#'
#' This class stores results from the BayeScan program.
#'
#' @slot fst \code{data.frame}
#' @slot mcmc \code{data.frame}
#' @slot acceptance.rate \code{data.frame}
#' @slot verification \code{data.frame}
#' @seealso \code{\link{BayeScanResults}}.
#' @export
setClass(
	"BayeScanResults",
	representation(
		fst='data.frame',
		mcmc='data.frame',
		acceptance.rate='data.frame',
		verification='character'
	),
	validity=function(object) {
		# fst
		expect_is(object@fst, 'data.frame')
		expect_equal(names(object@fst), c("loci", "prob", "log10_PO", "qval", "alpha", "fst", "type"))
		for (i in c("loci", "prob", "log10_PO", "qval", "alpha", "fst"))
			expect_is(object@fst[[i]], c('numeric','integer'))
		expect_is(object@fst[['type']], 'factor')
		# mcmc
		expect_is(object@mcmc, 'data.frame')
		for (i in names(object@mcmc))
			expect_is(object@mcmc[[i]], c('numeric','integer'))
		# acceptance.rate
		expect_is(object@acceptance.rate, 'data.frame')
		expect_equal(names(object@acceptance.rate), c("beta", "ances", "freq"))
		for (i in names(object@acceptance.rate))
			expect_is(object@acceptance.rate[[i]], c('numeric','integer'))
		# verification
		expect_is(object@verification, 'character')
		expect_equal(length(object@verification), 1L)
		return(TRUE)
	}
)

#' Create BayeScanResults object
#'
#' This function creates a new \code{BayeScanResults} object.
#'

#' @seealso \code{\link{BayeScanResults-class}}.
#' @return \code{\link{BayeScanResults}}.
#' @export
BayeScanResults<-function(fst, mcmc, acceptance.rate, verification) {
	x<-new("BayeScanResults", fst=fst, mcmc=mcmc, acceptance.rate=acceptance.rate, verification=verification)
	validObject(x, test=FALSE)
	return(x)
}

#' @rdname n.loci
#' @method n.loci BayeScanResults
#' @export
n.loci.BayeScanResults <- function(x) {
	return(nrow(x@fst))
}

#' @rdname n.pop
#' @method n.pop BayeScanResults
#' @export
n.pop.BayeScanResults <- function(x) {
	return((ncol(x@mcmc)-2)/2)
}

#' @rdname n.samples
#' @method n.samples BayeScanResults
#' @export
n.samples.BayeScanResults <- function(x) {
	stop('BayeScanResults does not store this information.')
	return(invisible())
}

#' @rdname pop.names
#' @method pop.names BayeScanResults
#' @export
pop.names.BayeScanResults <- function(x) {
	stop('BayeScanResults does not store population names.')
	return(invisible())
}

#' @rdname sample.pops
#' @method sample.pops BayeScanResults
#' @export
sample.pops.BayeScanResults <- function(x) {
	stop('BayeScanResults does not store population names.')
	return(invisivle())
}


#' Read BayeScan results
#'
#' This function reads the results the BayeScan program.
#'
#' @param file \code{character} file path of input file.
#' @param dir \code{dir} directory with output files.
#' @param threshold \code{numeric} threshold probability to classify loci as adaptive.
#' @seealso \code{\link{BayeScanResults-class}}.
#' @return \code{\link{BayeScanResults}}.
#' @export
read.BayeScanResults<-function(file, dir, threshold=0.95) {
return(
		BayeScanResults(
			fst=base::transform(
				`names<-`(
					fread(gsub('\\.txt', '_fst.txt', file.path(dir, basename(file))), skip=1, data.table=FALSE),
					c('loci','prob','log10_PO','qval','alpha','fst')
				),
				type=c('neutral','adaptive')[(prob>=threshold)+1]
			),
			mcmc=`names<-`(
				fread(gsub('\\.txt', '.sel', file.path(dir, basename(file))), skip=1, data.table=FALSE),
				c('iteration',strsplit(readLines(gsub('\\.txt', '.sel', file.path(dir, basename(file))),n=1),' ')[[1]])
			),
			acceptance.rate=fread(gsub('\\.txt', '_AccRte.txt', file.path(dir, basename(file))), data.table=FALSE),
			verification=paste(readLines(gsub('\\.txt', '_Verif.txt', file.path(dir, basename(file)))), collapse='\n')
		)
	)
}

#' @method print BayeScanResults
#' @rdname print
#' @export
print.BayeScanResults=function(x, ..., header=TRUE) {
	if (header)
		cat("BayeScanResults object.\n")
	cat('  adaptive loci:',sum(x@fst$type=='adaptive'),'\n')
	cat('  neutral loci:',sum(x@fst$type=='neutral'),'\n')
}

#' @rdname show
#' @export
setMethod(
	'show',
	'BayeScanResults',
	function(object)
		print.BayeScanResults(object)
)

