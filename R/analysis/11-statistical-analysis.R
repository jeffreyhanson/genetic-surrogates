## load .rda
session::restore.session('results/.cache/10-multi-species-prioritisations-with-cost.rda')
load('results/.cache/09-multi-species-prioritisations-no-cost.rda')
load('results/.cache/08-single-species-prioritisations.rda')
load('results/.cache/07-surrogacy-prioritizations.rda')

#### Statistical analysis
### surrogacy analysis
# prepare data
correlation.DF$Surrogate.target <- as.factor(correlation.DF$Surrogate.target)
correlation.DF$Surrogate.target.Method.Type <- with(correlation.DF, interaction(Surrogate.target, Method, Type))

# fit models
full.correlation.GLMM <- glmer(genetic.held ~ Surrogate.target*Method*Type + (1|Species), data=correlation.DF, family='binomial', nAGQ=10, control=glmerControl(optimizer=c('bobyqa'),optCtrl=list(maxfun=1e5)))
sub.correlation.GLMM <- drop1(full.correlation.GLMM, scope=~., test='Chisq')
null.correlation.GLMM <- update(full.correlation.GLMM, . ~ 1 + (1|Species))
# posthoc analysis
posthoc.correlation.GLMM <- glmer(genetic.held ~ Surrogate.target.Method.Type + (1|Species), data=correlation.DF, family='binomial', nAGQ=10, control=glmerControl(optimizer=c('bobyqa'),optCtrl=list(maxfun=1e5)))
posthoc.correlation.GLHT <- summary(
	glht(posthoc.correlation.GLMM,
		linfct=mcp(Surrogate.target.Method.Type='Tukey')),
	adjusted('bonferroni'))

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
# fit models
full.model.GLMM <- glmer(value ~ Prioritisation*Metric*Context + (1 | Species), family='binomial', data=results.SDF, nAGQ=10, control=glmerControl(optimizer=c('bobyqa'), optCtrl=list(maxfun=1e5)))
sub.model.GLMM <- drop1(full.model.GLMM, scope=~., test='Chisq')
null.model.GLMM <- update(full.model.GLMM, .~ 1 + (1|Species))

# post-hoc
posthoc.model.GLMM <- glmer(value ~ Prioritisation.Metric.Context + (1 | Species), family='binomial', data=results.SDF, nAGQ=10, control=glmerControl(optimizer=c('bobyqa'), optCtrl=list(maxfun=1e5)))
posthoc.model.GLHT <- summary(
	glht(posthoc.model.GLMM,
		linfct=mcp(Prioritisation.Metric.Context='Tukey')),
	adjusted('bonferroni'))

## save .rda
save.session('results/.cache/11-statistical-analysis.rda', compress='xz')
