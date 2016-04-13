## load .rda
session::restore.session('data/results/11-statistical-analysis.rda')

#### save results
session::save.session('results/results.rda', compress='xz')


