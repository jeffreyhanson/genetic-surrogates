## load .rda
checkpoint::checkpoint(general.params.LST[[MODE]]$checkpoint_date, R.version=general.params.LST[[MODE]]$checkpoint_R_version, scanForPackages=FALSE)
session::restore.session('data/intermediate/12-statistical-analysis.rda')

#### save results
session::save.session('data/final/results.rda', compress='xz')


