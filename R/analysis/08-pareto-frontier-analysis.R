## load .rda
session::restore.session('results/.cache/07-multi-species-prioritisations.rda')

## pareto frontier analysis
# generate prioritistions
cat("starting environmental pareto frontier calculations\n")
env.pareto.prioritisations <- llply(
	rapr.params.LST[[MODE]]$pareto.surrogate.targets,
	species.prioritisation,
	x=ru,
	amount.targets=0,
	geo.surrogate.targets=0,
	adaptive.genetic.targets=0,
	neutral.genetic.targets=0,
	.progress='text'
)

cat("starting geographic pareto frontier calculations\n")
geo.pareto.prioritisations <- llply(
	rapr.params.LST[[MODE]]$pareto.surrogate.targets,
	species.prioritisation,
	x=ru,
	amount.targets=0,
	env.surrogate.targets=0,
	adaptive.genetic.targets=0,
	neutral.genetic.targets=0,
	.progress='text'
)

# extract results
env.pareto.DF <- ldply(seq_along(env.pareto.prioritisations), function(i) {
	mutate(
		extractResults(env.pareto.prioritisations[[i]]),
		Surrogate.target=rapr.params.LST[[MODE]]$pareto.surrogate.targets[i]
	)
})
geo.pareto.DF <- ldply(seq_along(geo.pareto.prioritisations), function(i) {
	mutate(
		extractResults(geo.pareto.prioritisations[[i]]),
		Surrogate.target=rapr.params.LST[[MODE]]$pareto.surrogate.targets[i]
	)
})

## save .rda
save.session('results/.cache/08-pareto-frontier-analysis.rda')
