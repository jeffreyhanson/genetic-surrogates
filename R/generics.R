#' @include genetic.surrogates-internal.R misc.R

#' Number of loci
#'
#' This function returns the number of loci in a \code{BayeScan} object.
#' 
#' @param x \code{BayeScanData}, \code{BayeScan}.
#' @return \code{integer}
#' @export
n.loci <- function(x) UseMethod('n.loci')

#' Number of populations
#'
#' This function returns the number of populations in a \code{BayeScan} object.
#' 
#' @param x \code{BayeScanData}, \code{BayeScan}.
#' @return \code{integer}
#' @export
n.pop <- function(x) UseMethod('n.pop')

#' Number of samples
#'
#' This function returns the number of samples in a \code{BayeScan} object.
#' 
#' @param x \code{BayeScanData}, \code{BayeScan}.
#' @return \code{integer}
#' @export
n.samples <- function(x) UseMethod('n.samples')

#' Names of populations
#'
#' This function returns the unique population names in a \code{BayeScan} object.
#' 
#' @param x \code{BayeScanData}, \code{BayeScan}.
#' @return \code{character}
#' @export
pop.names <- function(x) UseMethod('pop.names')

#' Name of sample populations
#'
#' This function returns the population name that each sample belongs to in a \code{BayeScan} object.
#' 
#' @param x \code{BayeScanData}, \code{BayeScan}.
#' @return \code{character}
#' @export
sample.pops <- function(x) UseMethod('sample.pops')

#' Subset populations
#'
#' This function returns a subset of populations in a \code{BayeScan} object.
#' 
#' @param x \code{BayeScanData}, \code{BayeScan}.
#' @param populations \code{character} name of populations to subset.
#' @return \code{BayeScanData}
#' @export
pop.subset <- function(x, populations) UseMethod('pop.subset')

#' Subset loci
#'
#' This function returns a subset of loci in a \code{BayeScan} object.
#' 
#' @param x \code{BayeScanData}, or \code{BayeScan}.
#' @param type \code{numeric} index of loci to subset. If \code{x} is a \code{BayeScan} object, then a \code{character} denoting the type of loci to subset. Valid arguments are 'adaptive', or 'neutral'. 
#' @return \code{BayeScanData}
#' @export
loci.subset <- function(x, type) UseMethod('loci.subset')

#' Nonmetric multidimensional scaling for AFLP data
#'
#' This function performs nonmetric multidimensional scaling analysis on the loci in a \code{BayeScan} object.
#'
#' @param x \code{BayeScanData} object.
#' @param metric \code{character} name of distance metric to use. Valid arguments are 'euclidean', 'manhattan', or 'gower'. Defaults to 'gower'.
#' @param type \code{character} type of loci to analyse. Valid arguments are 'all', 'adaptive', or 'neutral'. Defaults to 'all'.
#' @param ... arguments passed to \code{link[vegan]{metaMDS}}.
#' @seealso \code{link[cluster]{daisy}}, \code{link[vegan]{metaMDS}}.
#' @return \code{\link[vegan]{metaMDS}} object.
#' @details The \code{link[cluster]{daisy}} function is used to calculate distances because it can accomodate missing values.
#' @examples
#' # run BayeScan using low number of iterations
#' dat <- read.BayeScanData('int/Data_Meirmans_et_al_IntrabioDiv/Androsace_obtusifolia_AFLP.dat')
#' x <- run.BayeScan(x, threads=1, n=50, thin=1, nbp=10, pilot=10, burn=10)
#' z <- mds(x, metric='gower' k=2)
#' @export
mds <- function(x, metric, type, ...) UseMethod('mds')


#' Print objects
#'
#' This function prints objects.
#'
#' @param x the object to print.
#' @param header \code{logical} should object header be shown?
#' @param ... not used.
#' @name print
NULL

#' Show objects
#'
#' This function shows objects.
#'
#' @param object the object to show.
#' @name show
NULL
