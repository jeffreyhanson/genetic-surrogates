test_that('run.BayeScan', {
	# make BayeScan object
	bd <- read.BayeScanData('../../inst/extdata/Data_Meirmans_et_al_IntrabioDiv/Androsace_obtusifolia_AFLP.dat')
	bs <- run.BayeScan(bd, threshold=0.5,threads=1,n=50,thin=1,nbp=10,pilot=50,burn=10)
	# methods
	mds(bs, metric='gower', k=2, type='neutral')
	print(bs)
	bs
})

