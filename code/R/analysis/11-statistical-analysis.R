## load .rda
session::restore.session('data/intermediate/10-multi-species-prioritisations-with-cost.rda')
load('data/intermediate/09-multi-species-prioritisations-no-cost.rda')
load('data/intermediate/08-single-species-prioritisations.rda')
load('data/intermediate/07-surrogacy-prioritizations.rda')

#### Statistical analyses
### correlation analysis
# species-level models
correlation.LST <- llply(
	seq_along(unique(spp.samples.sub.DF$species)),
	function(i) {
		## init
		curr.spp.DF <- select(grid.sub.DF, contains(unique(spp.samples.sub.DF$species)[i]))
		## prelim
		curr.spp.coords.LST <- list(
			select(grid.sub.DF, starts_with('geo_')),
			select(grid.sub.DF, starts_with('env_')),
			select(curr.spp.DF, contains('adaptive')),
			select(curr.spp.DF, contains('neutral'))
		)
		curr.spp.coords.LST <- llply(curr.spp.coords.LST, function(x) {
			return(x[!is.na(curr.spp.coords.LST[[4]][[1]]),])
		})
		curr.spp.dists.LST <- llply(curr.spp.coords.LST, dist) %>% llply(as.matrix)
		names(curr.spp.dists.LST) <- c('geo', 'env', 'adapt', 'neutral')
		curr.spp.dists.LST$null <- matrix(1, ncol=ncol(curr.spp.dists.LST[[1]]), nrow=nrow(curr.spp.dists.LST[[1]]))
		## main
		if (nrow(curr.spp.coords.LST[[3]])>0) {
			adapt.MLPE <- MLPE(x=curr.spp.dists.LST[['env']], y=curr.spp.dists.LST[['adapt']], REML=FALSE)
		} else {
			adapt.MLPE  <- NULL
		}
		neutral.MLPE <- MLPE(x=curr.spp.dists.LST[['geo']], y=curr.spp.dists.LST[['neutral']], REML=FALSE)
		## exports
		return(list(adapt.MLPE, neutral.MLPE))
	}
)

# extract statistics
correlation.DF <- ldply(
	seq_along(unique(spp.samples.sub.DF$species)),
	function(i) {
		if (!is.null(correlation.LST[[i]][[1]])) {
			env.ANOVA <- as.data.frame(anova(correlation.LST[[i]][[1]][[1]], correlation.LST[[i]][[1]][[2]]))
			env.df <- paste0(env.ANOVA$Df[[1]], ', ', env.ANOVA$Df[[2]])
			env.r2 <- r.squaredGLMM(correlation.LST[[i]][[1]][[1]])
		} else {
			env.ANOVA <- data.frame(F=c(NA, NA), Df=c(NA, NA), P=c(NA, NA))
			env.df <- NA
			env.r2 <- c(NA, NA)
		}
		geo.ANOVA <- as.data.frame(anova(correlation.LST[[i]][[2]][[1]], correlation.LST[[i]][[2]][[2]]))
		geo.df=paste0(geo.ANOVA$Df[[1]], ', ', geo.ANOVA$Df[[2]])
		geo.r2 <- r.squaredGLMM(correlation.LST[[i]][[2]][[1]])
		pvalues <- as.character(round(c(geo.ANOVA[['Pr(>Chisq)']][[2]], env.ANOVA[['Pr(>Chisq)']][[2]]),3))
		pvalues[which(as.numeric(pvalues) < 0.001)] <- '< 0.001'
		data.frame(
			species=rep(paste0('\\textit{',gsub('\\_', '\\ ', unique(spp.samples.sub.DF$species)[i]),'}'),2),
			Test=c('geographic vs neutral', 'environmental vs adaptive'),
			Marginal_R2=c(geo.r2[1], env.r2[1]),
			Conditional_R2=c(geo.r2[2], env.r2[2]),
			Chisq=c(geo.ANOVA$Chisq[[2]], env.ANOVA$Chisq[[2]]),
			P=pvalues
		)
	}
)

### surrogacy analyses
## prepare data
surrogacy.DF$Surrogate.target <- as.factor(surrogacy.DF$Surrogate.target)

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
	filter(!is.na(genetic.held)) %>% 
	summarize(count=sum(genetic.held>=rapr.params.LST[[MODE]]$scenario.analysis$genetic.target), total=length(genetic.held)) %>% 
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
save.session('data/intermediate/11-statistical-analysis.rda', compress='xz')
