## load .rda
session::restore.session('results/.cache/09-multi-species-prioritisations-with-cost.rda')

#### Statistical analysis
### surrogacy analysis
mod.form = ~ a - (a - 0) * exp(-r * input)
mod.fun = deriv(mod.form, namevec=c('a','r'), function.arg=c('input','a','r'))
env.correlation.NLMM <- nlmer(adaptive.held ~ mod.fun(Surrogate.target,a,r) ~ (a+r)|Species, data=env.correlation.DF, start=c(a=1,r=1), control=nlmerControl(optimizer='bobyqa'))
geo.correlation.NLMM <- nlmer(neutral.held ~ mod.fun(Surrogate.target,a,r) ~ (a+r)|Species, data=geo.correlation.DF, start=c(a=1,r=1), control=nlmerControl(optimizer='bobyqa'))

### test for differeces among scenarios
## prepare data
# single species prioritisations
single.spp.SDF <- single.spp.DF %>%
	gather(Metric, value, amount.held:neutral.held) %>%
	filter(Metric %in% c('adaptive.held', 'neutral.held')) %>%
	mutate(Context='single-species (equal costs)')
	
# multi-species prioritisations without cost
multi.spp.SDF <- multi.spp.DF %>%
	gather(Metric, value, amount.held:neutral.held) %>%
	filter(Metric %in% c('adaptive.held', 'neutral.held')) %>%
	mutate(Context='multi-species (equal costs)') 

# multi-species prioritisations with cost
multi.spp.with.cost.SDF <- multi.spp.with.cost.DF %>%
	gather(Metric, value, amount.held:neutral.held) %>%
	filter(Metric %in% c('adaptive.held', 'neutral.held')) %>%
	mutate(Context='multi-species (opportunity costs)') 

# compile data.frames
results.SDF <- rbind.fill(list(single.spp.SDF, multi.spp.SDF, multi.spp.with.cost.SDF)) %>%
	mutate(Metric=revalue(Metric, c('adaptive.held'='Adaptive variation', 'neutral.held'='Neutral variation'))) %>%
	mutate(Prioritisation = factor(Prioritisation, levels=c('Amount','Surrogate','Genetic'))) %>%
	mutate(Prioritisation.Metric.Context=interaction(Prioritisation,Metric,Context))

## main analysis
# model
full.model.GLMM <- glmer(value ~ Prioritisation.Metric.Context + (1 | Species), family='binomial', data=results.SDF, nAGQ=20)
null.model.GLMM <- glmer(value ~ 1 + (1 | Species), family='binomial', data=results.SDF, nAGQ=20)

## post-hoc
full.model.GLHT <- summary(
	glht(full.model.GLMM,
		linfct=mcp(Prioritisation.Metric.Context='Tukey')),
	adjusted('bonferroni'))
 
## save .rda
save.session('results/.cache/10-statistical-analysis.rda')
