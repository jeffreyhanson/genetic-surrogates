## load .rda
session::restore.session('results/.cache/11-statistical-analysis.rda')

#### save results
session::save.session('results/results.rda', compress='xz')


