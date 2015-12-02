test_that('BayeScanData', {
	# simualate data
	primers <- letters[1:5]
	populations <- LETTERS[1:10]
	matrix <- matrix(sample(0:1, size=50, replace=TRUE), ncol=5, nrow=10)
	matrix[sample(1:50, size=10, replace=FALSE)] <- NA
	# tests implicit
	x <- BayeScanData(
		matrix=matrix,
		primers=primers,
		populations=populations
	)
	# test methods
	expect_equal(n.pop(x), nrow(x@matrix))
	expect_equal(n.loci(x), ncol(x@matrix))
	expect_equal(pop.names(x), unique(x@populations))
	expect_equal(sample.pops(x), x@populations)
	print(x)
	x
})

test_that('read.BayeScanData', {
	in.bsd <- read.BayeScanData('../../inst/extdata/Data_Meirmans_et_al_IntrabioDiv/Androsace_obtusifolia_AFLP.dat')
})

test_that('write.BayeScanData', {
	path <- tempfile(fileext='.txt')
	bsd <- read.BayeScanData('../../inst/extdata/Data_Meirmans_et_al_IntrabioDiv/Androsace_obtusifolia_AFLP.dat')
	write.BayeScanData(bsd, path)
})

test_that('pop.subset.BayeScanData', {
	# load data
	bsd <- read.BayeScanData('../../inst/extdata/Data_Meirmans_et_al_IntrabioDiv/Androsace_obtusifolia_AFLP.dat')
	# subset data
	bsd2 <- pop.subset(bsd, as.character(1:10))
	# tests
	expect_equal(n.pop(bsd2), 10)
	expect_equal(pop.names(bsd2), as.character(1:10))
})

test_that('loci.subset.BayeScanData', {
	# load data
	bsd <- read.BayeScanData('../../inst/extdata/Data_Meirmans_et_al_IntrabioDiv/Androsace_obtusifolia_AFLP.dat')
	# subset data
	bsd2 <- loci.subset(bsd, 1:10)
	# tests
	expect_equal(n.loci(bsd2), 10)
	expect_equal(length(bsd2@primers), 10)
})

test_that('mds.BayeScanData', {
	# load data
	bsd <- read.BayeScanData('../../inst/extdata/Data_Meirmans_et_al_IntrabioDiv/Androsace_obtusifolia_AFLP.dat')
	# mds
	x<-mds(bsd)
})

