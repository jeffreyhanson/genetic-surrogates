## load .rda
session::restore.session('results/.cache/10-multi-species-prioritisations-with-cost.rda')
load('results/.cache/09-multi-species-prioritisations-no-cost.rda')
load('results/.cache/08-single-species-prioritisations.rda')
load('results/.cache/07-surrogacy-prioritizations.rda')

#### Statistical analyses
### surrogacy analyses
# prepare data
correlation.DF$Surrogate.target <- as.factor(correlation.DF$Surrogate.target)
correlation.DF$Surrogate.target.Type <- with(correlation.DF, interaction(Surrogate.target, Type))
correlation.sub.DF <- correlation.DF[rowSums(apply(select(correlation.DF, Surrogate.target, Type), 2, is.na))==0,]

# prepare matrix with success vs. failures
correlation.contingency.DF <- correlation.sub.DF %>% group_by(Surrogate.target,Type) %>% filter(!is.na(genetic.held)) %>% summarize(count=sum(genetic.held>=0.8), total=length(genetic.held)) %>% ungroup %>% mutate(prop=count/total, Surrogate.target2=as.numeric(as.character(Surrogate.target))) %>% mutate(secured=count, not.secured=total-count, name=paste0(Surrogate.target, '.', Type)) %>% select(secured, not.secured, name) %>% data.frame
correlation.MTX <- as.matrix(select(correlation.contingency.DF, secured, not.secured))
rownames(correlation.MTX) <- correlation.contingency.DF$name

# run fisher test
correlation.fisher.test <- fisher.test(correlation.MTX, workspace=1e8)

# run post-hoc fisher test
correlation.fisher.multcomp <- fisher.multcomp(correlation.MTX, 'bonferroni')
correlation.fisher.cld <- cld.RV.multcomp(correlation.fisher.multcomp)

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
scenario.DF <- rbind.fill(list(single.spp.SDF, multi.spp.SDF, multi.spp.with.cost.SDF)) %>%
	mutate(Metric=revalue(Metric, c('adaptive.held'='Adaptive variation', 'neutral.held'='Neutral variation'))) %>%
	mutate(Context=revalue(Context, c(
		'single-species (equal costs)'='Single-species\n(equal costs)',
		'multi-species (equal costs)'='Multi-species\n(equal costs)',
		'multi-species (opportunity costs)'='Multi-species\n(opportunity costs)'))) %>%
	mutate(Prioritisation=revalue(Prioritisation, c(
		'Amount'='Amount\ntargets',
		'Surrogate'='Amount & surrogate\ntargets',
		'Genetic'='Amount & genetic\ntargets'
	))) %>%
	mutate(
		Metric=factor(as.character(Metric), levels=c('Adaptive variation','Neutral variation')),
		Context=factor(as.character(Context), levels=c('Single-species\n(equal costs)','Multi-species\n(equal costs)','Multi-species\n(opportunity costs)')),
		Prioritisation=factor(as.character(Prioritisation), levels=c('Amount\ntargets','Amount & surrogate\ntargets','Amount & genetic\ntargets'))
	) %>%
	mutate(Prioritisation.Metric.Context=interaction(Prioritisation,Metric,Context))
	
# remove NA values
scenario.sub.DF <- scenario.DF[rowSums(apply(select(scenario.DF, Prioritisation, Metric, Context), 2, is.na))==0,]

# prepare matrix with success vs. failures
scenario.contingency.DF <- scenario.sub.DF %>% 
	group_by(Prioritisation.Metric.Context) %>% 
	filter(!is.na(value)) %>% 
	summarize(count=sum(value>=0.8), total=length(value)) %>% 
	ungroup %>% 
	mutate(prop=count/total, secured=count, not.secured=total-count) %>%
	select(secured, not.secured, Prioritisation.Metric.Context) %>%
	data.frame()

scenario.MTX <- as.matrix(select(scenario.contingency.DF, secured, not.secured))
rownames(scenario.MTX) <- scenario.contingency.DF$Prioritisation.Metric.Context

## main analysis
# fisher test
scenario.fisher.test <- fisher.test(scenario.MTX, workspace=1e9)

# post-hoc posthoc fisher tests
scenario.fisher.multcomp <- fisher.multcomp(scenario.MTX, 'bonferroni')

scenario.fisher.cld <- cld.RV.multcomp(scenario.fisher.multcomp)

## save .rda
save.session('results/.cache/11-statistical-analysis.rda', compress='xz')
