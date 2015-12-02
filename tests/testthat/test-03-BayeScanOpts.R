test_that('BayeScanOpts', {
	# tests implicit
	x <- BayeScanOpts(threads=1, n=5000, thin=10, nbp=20, pilot=5000, burn=50000, threshold=0.95)
	# methods
	print(x)
	x
})

