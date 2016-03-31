## load .rda
session::restore.session('results/.cache/10-multi-species-prioritisations-with-cost.rda')
load('results/.cache/09-multi-species-prioritisations-no-cost.rda')
load('results/.cache/08-single-species-prioritisations.rda')
load('results/.cache/07-surrogacy-prioritizations.rda')

#### Statistical analyses
### surrogacy analyses
# prepare data
correlation.DF$Surrogate.target <- as.factor(correlation.DF$Surrogate.target)
correlation.DF$Type.Surrogate.target.Method <- with(correlation.DF, interaction(Type, Surrogate.target, Method))
correlation.sub.DF <- correlation.DF[rowSums(apply(select(correlation.DF, Type, Surrogate.target, Method), 2, is.na))==0,]

# run models
full.correlation.GLMM <- glmer(genetic.held~Type.Surrogate.target.Method + (1|Species), data=correlation.sub.DF, family='binomial', nAGQ=10, control=glmerControl(optimizer=c('bobyqa'),optCtrl=list(maxfun=1e5)))
null.correlation.GLMM <- glmer(genetic.held~ 1 + (1|Species), data=correlation.sub.DF, family='binomial', nAGQ=10, control=glmerControl(optimizer=c('bobyqa'),optCtrl=list(maxfun=1e5)))

# compare models
correlation.ANOVA <- anova(full.correlation.GLMM, null.correlation.GLMM, test='Chisq')

# posthoc analysis
correlation.GLHT <- summary(
	glht(full.correlation.GLMM, linfct=mcp(Type.Surrogate.target.Method='Tukey')),
	adjusted('bonferroni')
)

### scenario analyses
## prepare data
# single species prioritisations
single.spp.SDF <- single.spp.DF %>%
	gather(Metric, genetic.held, amount.held:neutral.held) %>%
	filter(Metric %in% c('adaptive.held', 'neutral.held')) %>%
	mutate(Context='single-species (equal costs)')
	
# multi-species prioritisations without cost
multi.spp.SDF <- multi.spp.DF %>%
	gather(Metric, genetic.held, amount.held:neutral.held) %>%
	filter(Metric %in% c('adaptive.held', 'neutral.held')) %>%
	mutate(Context='multi-species (equal costs)') 

# multi-species prioritisations with cost
multi.spp.with.cost.SDF <- multi.spp.with.cost.DF %>%
	gather(Metric, genetic.held, amount.held:neutral.held) %>%
	filter(Metric %in% c('adaptive.held', 'neutral.held')) %>%
	mutate(Context='multi-species (opportunity costs)') 

# compile data.frames
scenario.DF <- rbind.fill(list(single.spp.SDF, multi.spp.SDF, multi.spp.with.cost.SDF)) %>%
	mutate(Metric=revalue(Metric, c('adaptive.held'='Adaptive variation', 'neutral.held'='Neutral variation'))) %>%
	mutate(Context=revalue(Context, c(
		'single-species (equal costs)'='Single-species\n(equal costs)',
		'multi-species (equal costs)'='Multi-species\n(equal costs)',
		'multi-species (opportunity costs)'='Multi-species\n(opportunity costs)'))) %>%
	mutate(Prioritisation=revalue(Prioritisation, c(
		'Amount'='Amount targets',
		'Surrogate'='Amount & surrogate\ntargets',
		'Genetic'='Amount & genetic\ntargets'
	))) %>%
	mutate(
		Metric=factor(as.character(Metric), levels=c('Adaptive variation','Neutral variation')),
		Context=factor(as.character(Context), levels=c('Single-species\n(equal costs)','Multi-species\n(equal costs)','Multi-species\n(opportunity costs)')),
		Prioritisation=factor(as.character(Prioritisation), levels=c('Amount targets','Amount & surrogate\ntargets','Amount & genetic\ntargets'))
	) %>%
	mutate(Prioritisation.Metric.Context=interaction(Prioritisation,Metric,Context))
	
# remove NA values
scenario.sub.DF <- scenario.DF[rowSums(apply(select(scenario.DF, Prioritisation, Metric, Context), 2, is.na))==0,]

# run models
full.scenario.GLMM <- glmer(genetic.held~Prioritisation.Metric.Context + (1|Species), data=scenario.sub.DF, family='binomial', nAGQ=10, control=glmerControl(optimizer=c('bobyqa'),optCtrl=list(maxfun=1e5)))
null.scenario.GLMM <- glmer(genetic.held~ 1 + (1|Species), data=scenario.sub.DF, family='binomial', nAGQ=10, control=glmerControl(optimizer=c('bobyqa'),optCtrl=list(maxfun=1e5)))

# compare models
scenario.ANOVA <- anova(full.scenario.GLMM, null.scenario.GLMM, test='Chisq')

# posthoc analysis
scenario.GLHT <- summary(
	glht(full.scenario.GLMM, linfct=mcp(Prioritisation.Metric.Context ='Tukey')),
	adjusted('bonferroni')
)

## save .rda
save.session('results/.cache/11-statistical-analysis.rda', compress='xz')
