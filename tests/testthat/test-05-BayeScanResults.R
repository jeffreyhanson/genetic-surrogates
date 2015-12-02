test_that('read.BayeScanResults', {
	# create data
	dir<-tempdir()
	path<-tempfile(tmpdir=dir, fileext='.txt')
	bo <-BayeScanOpts(threshold=0.5,threads=1,n=50,thin=1,nbp=10,pilot=50,burn=10)
	bd <- read.BayeScanData('../../inst/extdata/Data_Meirmans_et_al_IntrabioDiv/Androsace_obtusifolia_AFLP.dat')
	write.BayeScanData(bd,path)
	# identify bayescan path
	bayescan.path <- switch(
		Sys.info()['sysname'],
		'Linux'=system.file('bin', 'bayescan_2.1', package='genetic.surrogates'),
		'Darwin'=system.file('bin', 'BayeScan2.1_macos64bits', package='genetic.surrogates'),
		'Windows'=system.file('bin', 'BayeScan2.1_win32bits_cmd_line.exe', package='genetic.surrogates')
	)
	# run BayeScan
	system(
		paste0(
			bayescan.path, ' ',
			path,
			' -od ',dir,
			' -threads ',bo@threads,
			' -n ',bo@n,
			' -thin ',bo@thin,
			' -nbp ',bo@nbp,
			' -pilot ',bo@pilot,
			' -burn ',bo@burn
		)
	)
	# try reading results back into R
	results <- read.BayeScanResults(path,dir,bo@threshold)
	# methods
	print(results)
	results
})
