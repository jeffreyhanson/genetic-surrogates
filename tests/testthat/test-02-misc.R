test_that('buffered.range', {
	# tests
	expect_equal(buffered.range(c(0,0.5,1), 0.05), c(-0.05,1.05))
})

test_that('spFortify', {
	# load data
	data(countriesHigh)
	# manually create fortified version
	countriesHigh.PLY <- countriesHigh[1:5,]
	countriesHigh.PLY$fid = seq_len(nrow(countriesHigh.PLY@data))
	countriesHigh.FPLY <- ggplot2::fortify(countriesHigh.PLY, region='fid')
	countriesHigh.FPLY <- merge(countriesHigh.FPLY, countriesHigh.PLY, by.x='id', by.y='fid')
	countriesHigh.FPLY <- countriesHigh.FPLY[,names(countriesHigh.FPLY)!='fid'] 
	# tests
	expect_identical(
		spFortify(countriesHigh[1:5,]),
		countriesHigh.FPLY
	)
})


