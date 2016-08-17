## load .rda
checkpoint::checkpoint('2016-08-03', R.version='3.3.0', scanForPackages=FALSE)
session::restore.session('data/intermediate/12-statistical-analysis.rda')

#### save results
session::save.session('data/final/results.rda', compress='xz')


