## load .rda
session::restore.session('results/.cache/07-multi-species-prioritisations.rda')

## correlation analysis
# generate prioritistions
cat("starting environmental prioritizations\n")
env.correlation.prioritisations <- llply(
	rapr.params.LST[[MODE]]$correlation.surrogate.targets,
	species.prioritisation,
	x=ru,
	amount.targets=0,
	geo.surrogate.targets=0,
	adaptive.genetic.targets=0,
	neutral.genetic.targets=0,
	.progress='text'
)

cat("starting geographic prioritizations\n")
geo.correlation.prioritisations <- llply(
	rapr.params.LST[[MODE]]$correlation.surrogate.targets,
	species.prioritisation,
	x=ru,
	amount.targets=0,
	env.surrogate.targets=0,
	adaptive.genetic.targets=0,
	neutral.genetic.targets=0,
	.progress='text'
)

# extract results
env.correlation.DF <- ldply(seq_along(env.correlation.prioritisations), function(i) {
	mutate(
		extractResults(env.correlation.prioritisations[[i]]),
		Surrogate.target=rapr.params.LST[[MODE]]$correlation.surrogate.targets[i]
	)
})
geo.correlation.DF <- ldply(seq_along(geo.correlation.prioritisations), function(i) {
	mutate(
		extractResults(geo.correlation.prioritisations[[i]]),
		Surrogate.target=rapr.params.LST[[MODE]]$correlation.surrogate.targets[i]
	)
})

## save .rda
save.session('results/.cache/08-correlation-analysis.rda')
