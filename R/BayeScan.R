#' @include genetic.surrogates-internal.R misc.R BayeScanOpts.R BayeScanData.R BayeScanResults.R

#' BayeScan: An S4 class to represent inputs and outputs from BayeScan
#'
#' This class stores input data and associated output results from the BayeScan program.
#'
#' @slot opts \code{BayeScanOpts} object with parameters used to run BayeScan .
#' @slot data \code{BayeScanData} object with input data used for analysis.
#' @slot results \code{BayeScanResults} object with results from analysis.
#' @seealso \code{\link{BayeScan}}.
#' @export
setClass(
	"BayeScan",
	representation(
		opts='BayeScanOpts',
		data='BayeScanData',
		results='BayeScanResults'
	),
	validity=function(object) {
		## opts
		# checks are internal
		## data
		# checks are internal
		## results
		# checks are internal
		## cross-object checks
		expect_equal(n.pop(object@data), n.pop(object@results))
		expect_equal(n.loci(object@data), n.loci(object@results))
		return(TRUE)
	}
)

#' Create BayeScan object
#'
#' This function creates a new \code{BayeScan} object.
#'
#' @param opts \code{BayeScanOpts} object with parameters used to run BayeScan .
#' @param data \code{BayeScanData} object with input data used for analysis.
#' @param results \code{BayeScanResults} object with results from analysis.
#' @seealso \code{\link{BayeScan-class}}, \code{\link{BayeScanData}}, \code{\link{BayeScanData}}, \code{\link{BayeScanResults}}.
#' @export
BayeScan<-function(opts, data, results) {
	x<-new("BayeScan", opts=opts, data=data, results=results)
	validObject(x, test=FALSE)
	return(x)
}

#' @rdname n.loci
#' @method n.loci BayeScan
#' @export
n.loci.BayeScan <- function(x) {
	return(n.loci(x@data))
}

#' @rdname n.pop
#' @method n.pop BayeScan
#' @export
n.pop.BayeScan <- function(x) {
	return(n.pop(x@data))
}

#' @rdname n.samples
#' @method n.samples BayeScan
#' @export
n.samples.BayeScan <- function(x) {
	return(n.samples(x@data))
}

#' @rdname pop.names
#' @method pop.names BayeScan
#' @export
pop.names.BayeScan <- function(x) {
	return(pop.names(x@data))
}

#' @rdname sample.pops
#' @method sample.pops BayeScan
#' @export
sample.pops.BayeScan <- function(x) {
	return(sample.pops(x@data))
}

#' @rdname loci.subset
#' @method loci.subset BayeScan
#' @export
loci.subset.BayeScan <- function(x, loci) {
	if (is.character(loci)) {
		if (loci=='all') {
			return(x@data)
		} else {
			return(loci.subset(x@data, x@results@fst$type==loci))
		}
	} else {
		return(loci.subset(x@data, loci))
	}
}


#' Run BayeScan
#'
#' This function analysis data using BayeScan.
#'
#' @param x \code{BayeScanData} object.
#' @inheritParams BayeScanOpts
#' @param dir \code{character} with directory to use for analysis.
#' @param clean \code{logical} should input and output files be deleted after analysis is finished?
#' @seealso \code{BayeScanData}, \code{BayeScanOpts}.
#' @examples
#' # run BayeScan using low number of iterations
#' dat <- read.BayeScanData('int/Data_Meirmans_et_al_IntrabioDiv/Androsace_obtusifolia_AFLP.dat')
#' x <- run.BayeScan(x, threads=1, n=50, thin=1, nbp=10, pilot=10, burn=10)
#' @export
run.BayeScan<-function(x, threads=1, n=5000, thin=10, nbp=20, pilot=5000, burn=50000, threshold=0.95, dir=tempdir(), clean=TRUE) {
	## initialization
	# argument checks
	opts <- BayeScanOpts(threads=threads, n=n, thin=thin, nbp=nbp, pilot=pilot, burn=burn, threshold=threshold)
	expect_is(x, 'BayeScanData')
	# set BayeScan file path
	bayescan.path <- switch(
		Sys.info()['sysname'],
		'Linux'=system.file('bin', 'bayescan_2.1', package='genetic.surrogates'),
		'Darwin'=system.file('bin', 'BayeScan2.1_macos64bits', package='genetic.surrogates'),
		'Windows'=system.file('bin', 'BayeScan2.1_win32bits_cmd_line.exe', package='genetic.surrogates')
	)
	### main processing
	# write data to file
	dat.path <- tempfile(tmpdir=dir, fileext='.txt')
	write.BayeScanData(x, dat.path)
	# run BayesScan analysis
	system(
		paste0(
			bayescan.path, ' ',
			dat.path,
			' -od ',dir,
			' -threads ',opts@threads,
			' -n ',opts@n,
			' -thin ',opts@thin,
			' -nbp ',opts@nbp,
			' -pilot ',opts@pilot,
			' -burn ',opts@burn
		)
	)
	## exports
	# construct BayeScan object
	return(
		BayeScan(
			opts=opts,
			data=x,
			results=read.BayeScanResults(dat.path,dir,threshold=opts@threshold)
		)
	)
}

#' @method mds BayeScan
#' @rdname mds
#' @export
mds.BayeScan <- function(x, metric, type, ...) {
	return(
		mds.BayeScanData(
			loci.subset(x, type),
			metric,
			...
		)
	)
}

#' @method print BayeScan
#' @rdname print
#' @export
print.BayeScan=function(x, ..., header=TRUE) {
	if (header)
		cat("BayeScan object.\n\n")
	cat('Options','\n')
	print(x@data, header=FALSE)
	cat('Data','\n')
	print(x@data, header=FALSE)
	cat('Results','\n')
	print(x@results, header=FALSE)
}

#' @rdname show
#' @export
setMethod(
	'show',
	'BayeScan',
	function(object)
		print.BayeScan(object)
)

