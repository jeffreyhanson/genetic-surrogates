## load .rda
session::restore.session('results/.cache/08-pareto-frontier-analysis.rda')

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
single.spp.GLM <- suppressWarnings(glm(value ~ Prioritisation * Metric,
	family='binomial', data=single.spp.SDF))
single.spp.AOV <- suppressWarnings(anova(single.spp.GLM, test="LRT"))
# post-hoc
single.spp.GLM2 <- suppressWarnings(glm(value ~ Prioritisation.Metric,
	family='binomial', data=single.spp.SDF))
single.spp.MCP <- summary(
	glht(single.spp.GLM2,
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
multi.spp.GLM <- suppressWarnings(glm(value ~ Prioritisation * Metric,
	family='binomial', data=multi.spp.SDF))
multi.spp.AOV <- suppressWarnings(anova(multi.spp.GLM, test="LRT"))
# post-hoc
multi.spp.GLM2 <- suppressWarnings(glm(value ~ Prioritisation.Metric,
	family='binomial', data=multi.spp.SDF))
multi.spp.MCP <- summary(
	glht(multi.spp.GLM2,
		linfct=mcp(Prioritisation.Metric='Tukey')),
	adjusted('bonferroni'))

## save .rda
save.session('results/.cache/09-statistical-analysis.rda')
