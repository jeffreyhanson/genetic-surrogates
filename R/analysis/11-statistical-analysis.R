## load .rda
session::restore.session('results/.cache/10-multi-species-prioritisations-with-cost.rda')
load('results/.cache/09-multi-species-prioritisations-no-cost.rda')
load('results/.cache/08-single-species-prioritisations.rda')
load('results/.cache/07-surrogacy-prioritizations.rda')

#### Statistical analyses
### surrogacy analyses
# none because significance can be acheived by generating sufficient random reps 

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

# prepare matrix with success vs. failures
scenario.contingency.DF <- scenario.sub.DF %>% 
	group_by(Prioritisation.Metric.Context) %>% 
	filter(!is.na(value)) %>% 
	summarize(count=sum(value>=rapr.params.LST[[MODE]]$scenario.analysis$genetic.target), total=length(value)) %>% 
	ungroup %>% 
	mutate(prop=count/total, secured=count, not.secured=total-count) %>%
	select(secured, not.secured, Prioritisation.Metric.Context) %>%
	data.frame()

scenario.MTX <- as.matrix(select(scenario.contingency.DF, secured, not.secured))
rownames(scenario.MTX) <- scenario.contingency.DF$Prioritisation.Metric.Context

# run global test
# fisher test
scenario.G.test <- G.test(scenario.MTX)

# posthoc tests
scenario.G.multcomp <- pairwise.G.test(scenario.MTX, 'bonferroni')
scenario.G.cld <- cld.RV.multcomp(scenario.G.multcomp)

## save .rda
save.session('results/.cache/11-statistical-analysis.rda', compress='xz')
