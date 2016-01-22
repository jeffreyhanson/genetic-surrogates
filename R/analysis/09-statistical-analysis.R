## load .rda
session::restore.session('results/.cache/08-correlation-analysis.rda')

### Single species prioritisations
## statistical analysis
# prepare data
single.spp.SDF <- single.spp.DF %>%
	gather(Metric, value, amount.held:neutral.held) %>%
	filter(Metric %in% c('adaptive.held', 'neutral.held')) %>%
	mutate(Metric=revalue(Metric, c('adaptive.held'='Adaptive variation', 
		'neutral.held'='Neutral variation'))) %>%
	mutate(Prioritisation.Metric=interaction(Prioritisation,Metric)) 
# model
single.spp.GLMM <- suppressWarnings(glmer(value ~ Prioritisation * Metric + (1 | Species), family='binomial', data=single.spp.SDF, nAGQ=20))
single.spp.GLMM1 <- suppressWarnings(glmer(value ~ Prioritisation + Metric + (1 | Species), family='binomial', data=single.spp.SDF, nAGQ=20))
single.spp.GLMM2 <- suppressWarnings(glmer(value ~ Prioritisation + (1 | Species), family='binomial', data=single.spp.SDF, nAGQ=20))
single.spp.GLMM3 <- suppressWarnings(glmer(value ~ 1 + (1 | Species), family='binomial', data=single.spp.SDF, nAGQ=20))

single.spp.AOV <- suppressWarnings(anova(single.spp.GLMM, test="LRT"))
single.spp.AOV1 <- suppressWarnings(anova(single.spp.GLMM, single.spp.GLMM1, test="LRT"))
single.spp.AOV2 <- suppressWarnings(anova(single.spp.GLMM1, single.spp.GLMM2, test="LRT"))
single.spp.AOV3 <- suppressWarnings(anova(single.spp.GLMM2, single.spp.GLMM3, test="LRT"))

# post-hoc
single.spp.GLMM2 <- suppressWarnings(glmer(value ~ Prioritisation.Metric + (1 | Species), family='binomial', data=single.spp.SDF, nAGQ=20))
single.spp.MCP <- summary(
	glht(single.spp.GLMM2,
		linfct=mcp(Prioritisation.Metric='Tukey')),
	adjusted('bonferroni'))
 

### Multi-species prioritisations
## statistical analysis
# prepare data
multi.spp.SDF <- multi.spp.DF %>%
	gather(Metric, value, amount.held:neutral.held) %>%
	filter(Metric %in% c('adaptive.held', 'neutral.held')) %>%
	mutate(Metric=revalue(Metric, c('adaptive.held'='Adaptive variation', 
		'neutral.held'='Neutral variation'))) %>%
	mutate(Prioritisation.Metric=interaction(Prioritisation,Metric)) 
# model
multi.spp.GLMM <- suppressWarnings(glmer(value ~ Prioritisation * Metric + (1 | Species), family='binomial', data=multi.spp.SDF, nAGQ=20))
multi.spp.GLMM1 <- suppressWarnings(glmer(value ~ Prioritisation + Metric + (1 | Species), family='binomial', data=multi.spp.SDF, nAGQ=20))
multi.spp.GLMM2 <- suppressWarnings(glmer(value ~ Prioritisation + (1 | Species), family='binomial', data=multi.spp.SDF, nAGQ=20))
multi.spp.GLMM3 <- suppressWarnings(glmer(value ~ 1 + (1 | Species), family='binomial', data=multi.spp.SDF, nAGQ=20))

multi.spp.AOV <- suppressWarnings(anova(multi.spp.GLMM, test="LRT"))
multi.spp.AOV1 <- suppressWarnings(anova(multi.spp.GLMM, multi.spp.GLMM1, test="LRT"))
multi.spp.AOV2 <- suppressWarnings(anova(multi.spp.GLMM1, multi.spp.GLMM2, test="LRT"))
multi.spp.AOV3 <- suppressWarnings(anova(multi.spp.GLMM2, multi.spp.GLMM3, test="LRT"))

# post-hoc
multi.spp.GLMM2 <- suppressWarnings(glmer(value ~ Prioritisation.Metric + (1 | Species), family='binomial', data=multi.spp.SDF, nAGQ=20))
multi.spp.MCP <- summary(
	glht(multi.spp.GLMM2,
		linfct=mcp(Prioritisation.Metric='Tukey')),
	adjusted('bonferroni'))

### Correlation analysis
mod.form = ~ 0 + (1 - 0) * (1 - exp(-r * input))
mod.fun = deriv(mod.form, namevec=c('r'), function.arg=c('input','r'))
env.correlation.NLMM <- nlmer(adaptive.held ~ mod.fun(Surrogate.target,r) ~ r|Species, data=env.correlation.DF, start=c(r=1), control=nlmerControl(optimizer='bobyqa'))
geo.correlation.NLMM <- nlmer(neutral.held ~ mod.fun(Surrogate.target,r) ~ r|Species, data=env.correlation.DF, start=c(r=1), control=nlmerControl(optimizer='bobyqa'))

## save .rda
save.session('results/.cache/09-statistical-analysis.rda')
