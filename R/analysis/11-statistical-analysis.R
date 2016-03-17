## load .rda
session::restore.session('results/.cache/10-multi-species-prioritisations-with-cost.rda')
load('results/.cache/09-multi-species-prioritisations-no-cost.rda')
load('results/.cache/08-single-species-prioritisations.rda')
load('results/.cache/07-surrogacy-prioritizations.rda')

#### Statistical analyses
### surrogacy analyses
# prepare data
correlation.DF$Surrogate.target <- as.factor(correlation.DF$Surrogate.target)
correlation.DF$Surrogate.target.Method.Type <- with(correlation.DF, interaction(Surrogate.target, Method, Type))
correlation.sub.DF <- correlation.DF[rowSums(apply(select(correlation.DF, Surrogate.target, Method, Type), 2, is.na))==0,]

# fit models
full.correlation.GLMM <- glmer(genetic.held ~ Surrogate.target*Method*Type + (1|Species), data=correlation.sub.DF, family='binomial', nAGQ=10, control=glmerControl(optimizer=c('bobyqa'),optCtrl=list(maxfun=1e5)))
sub.correlation.1.GLMM <- update(full.correlation.GLMM, .~. -Surrogate.target:Method:Type)
sub.correlation.2.GLMM <- update(full.correlation.GLMM, .~. -Method:Type)
sub.correlation.3.GLMM <- update(full.correlation.GLMM, .~. -Surrogate.target:Type)
sub.correlation.4.GLMM <- update(full.correlation.GLMM, .~. -Surrogate.target:Method)
sub.correlation.5.GLMM <- update(full.correlation.GLMM, .~. -Type)
sub.correlation.6.GLMM <- update(full.correlation.GLMM, .~. -Method)
null.correlation.GLMM <- update(full.correlation.GLMM, .~. -Surrogate.target)

# compare models
sub.correlation.1.ANOVA <- anova(full.correlation.GLMM, sub.correlation.1.GLMM, test='Chisq')
sub.correlation.2.ANOVA <- anova(sub.correlation.1.GLMM, sub.correlation.2.GLMM, test='Chisq')
sub.correlation.3.ANOVA <- anova(sub.correlation.2.GLMM, sub.correlation.3.GLMM, test='Chisq')
sub.correlation.4.ANOVA <- anova(sub.correlation.3.GLMM, sub.correlation.4.GLMM, test='Chisq')
sub.correlation.5.ANOVA <- anova(sub.correlation.4.GLMM, sub.correlation.5.GLMM, test='Chisq')
sub.correlation.6.ANOVA <- anova(sub.correlation.5.GLMM, sub.correlation.6.GLMM, test='Chisq')
sub.correlation.7.ANOVA <- anova(sub.correlation.6.GLMM, null.correlation.GLMM, test='Chisq')

# posthoc analysis
posthoc.correlation.GLMM <- glmer(genetic.held ~ Surrogate.target.Method.Type + (1|Species), data=correlation.sub.DF, family='binomial', nAGQ=10, control=glmerControl(optimizer=c('bobyqa'),optCtrl=list(maxfun=1e5)))
posthoc.correlation.GLHT <- summary(
	glht(posthoc.correlation.GLMM,
		linfct=mcp(Surrogate.target.Method.Type='Tukey')),
	adjusted('bonferroni'))

### scenario analyses
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

# remove NA values
results.sub.SDF <- results.SDF[rowSums(apply(select(results.SDF, Prioritisation, Metric, Context), 2, is.na))==0,]

## main analysis
# fit models
full.scenario.GLMM <- glmer(value ~ Prioritisation*Metric*Context + (1 | Species), family='binomial', data=results.sub.SDF, nAGQ=10, control=glmerControl(optimizer=c('bobyqa'), optCtrl=list(maxfun=1e5)))
sub.scenario.1.GLMM <- update(full.model.GLMM, .~. -Prioritisation:Metric:Context)
sub.scenario.2.GLMM <- update(full.model.GLMM, .~. -Metric:Context)
sub.scenario.3.GLMM <- update(full.model.GLMM, .~. -Prioritisation:Context)
sub.scenario.4.GLMM <- update(full.model.GLMM, .~. -Prioritisation:Metric)
sub.scenario.5.GLMM <- update(full.model.GLMM, .~. -Context)
sub.scenario.6.GLMM <- update(full.model.GLMM, .~. -Metric)
null.scenario.GLMM <- update(full.model.GLMM, .~. -Prioritisation)

# compare models
sub.scenario.1.ANOVA <- anova(full.scenario.GLMM, sub.scenario.1.GLMM, test='Chisq')
sub.scenario.2.ANOVA <- anova(sub.scenario.1.GLMM, sub.scenario.2.GLMM, test='Chisq')
sub.scenario.3.ANOVA <- anova(sub.scenario.2.GLMM, sub.scenario.3.GLMM, test='Chisq')
sub.scenario.4.ANOVA <- anova(sub.scenario.3.GLMM, sub.scenario.4.GLMM, test='Chisq')
sub.scenario.5.ANOVA <- anova(sub.scenario.4.GLMM, sub.scenario.5.GLMM, test='Chisq')
sub.scenario.6.ANOVA <- anova(sub.scenario.5.GLMM, sub.scenario.6.GLMM, test='Chisq')
sub.scenario.7.ANOVA <- anova(sub.scenario.6.GLMM, null.scenario.GLMM, test='Chisq')

# post-hoc
posthoc.model.GLMM <- glmer(value ~ Prioritisation.Metric.Context + (1 | Species), family='binomial', data=results.sub.SDF, nAGQ=10, control=glmerControl(optimizer=c('bobyqa'), optCtrl=list(maxfun=1e5)))
posthoc.model.GLHT <- summary(
	glht(posthoc.model.GLMM,
		linfct=mcp(Prioritisation.Metric.Context='Tukey')),
	adjusted('bonferroni'))

## save .rda
save.session('results/.cache/11-statistical-analysis.rda', compress='xz')
