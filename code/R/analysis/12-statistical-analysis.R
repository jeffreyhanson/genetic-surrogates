## load .rda
checkpoint::checkpoint('2016-08-03', R.version='3.3.1', scanForPackages=FALSE)
session::restore.session('data/intermediate/11-multi-species-prioritisations-with-cost.rda')
load('data/intermediate/10-multi-species-prioritisations-no-cost.rda')
load('data/intermediate/09-single-species-prioritisations-with-cost.rda')
load('data/intermediate/08-single-species-prioritisations-no-cost.rda')
load('data/intermediate/07-surrogacy-prioritizations.rda')

#### Statistical analyses
### compute statistics reported for methods
loci_classification.DF <- ldply(
	seq_along(unique(spp.samples.DF$species)), 
	function(i) {
		if (!is.null(spp.BayeScan.LST[[i]])) {
			data.frame(
				Species=paste0('\\textit{',gsub('\\_', ' ', unique(spp.samples.DF$species)[i]),'}'),
				Primer=spp.BayeScan.LST[[i]]$bayescan@data@primers,
				
				Bayescan_fst=spp.BayeScan.LST[[i]]$bayescan@results@summary[[6]],
				Bayescan_Probability=spp.BayeScan.LST[[i]]$bayescan@results@summary[[2]],
				Bayescan_alpha=spp.BayeScan.LST[[i]]$bayescan@results@summary[[5]],
				Bayescan_qvalue=spp.BayeScan.LST[[i]]$bayescan@results@summary[[4]],
				Bayescan_Type=spp.BayeScan.LST[[i]]$bayescan@results@summary[[7]],
				
				PCadapt_K=spp.pcadapt.LST[[i]]$K,
				PCadapt_chi2_statistic=spp.pcadapt.LST[[i]]$inference.run$chi2.stat,
				PCadapt_pvalue=spp.pcadapt.LST[[i]]$pvalues,
				PCadapt_qvalue=spp.pcadapt.LST[[i]]$qvalues$qvalues,
				PCadapt_Type=replace(rep('neutral', length(spp.pcadapt.LST[[i]]$pvalues)), spp.pcadapt.LST[[i]]$adaptive.loci, 'adaptive'),
				
				Overall_Type=replace(rep('neutral', length(spp.pcadapt.LST[[i]]$pvalues)), spp.OutlierDetectionResults.LST[[i]], 'adaptive')
			)
		}
	}
)

spp.outlier.methods.congruence <- loci_classification.DF %>% 
	group_by(Species) %>%
	summarize(prop = (sum(Bayescan_Type==PCadapt_Type) / length(Species))*100) %>%
	ungroup() %>%
	select(prop) %>%
	data.frame() %>%
	`[[`(1)

spp.outlier.loci.props <- loci_classification.DF %>% 
	group_by(Species) %>%
	summarize(prop = (sum(Overall_Type=='adaptive') / length(Species))*100) %>%
	ungroup() %>%
	filter(prop > 0) %>%
	select(prop) %>%
	data.frame() %>%
	`[[`(1)
	
	
spp.nmds.summary.DF <- ldply(
	seq_along(spp.nmds.LST),
	function(i) {
		ldply(
			seq_along(spp.nmds.LST[[i]]),
			function(j) {
				data.frame(
					Species=paste0('\\textit{',gsub('\\_', ' ', unique(spp.samples.DF$species)[i]),'}'),
					Loci=names(spp.nmds.LST[[i]])[j],
					k=ifelse(is.null(spp.nmds.LST[[i]][[j]]), NA_integer_, spp.nmds.LST[[i]][[j]]$ndim),
					Stress=ifelse(is.null(spp.nmds.LST[[i]][[j]]), NA_real_, spp.nmds.LST[[i]][[j]]$stress),
					Converged=ifelse(is.null(spp.nmds.LST[[i]][[j]]), NA, spp.nmds.LST[[i]][[j]]$converged)
				)
			}
		)
	}
)

adaptive.k <- spp.nmds.summary.DF %>%
	filter(Loci=='adaptive') %>%
	select(k) %>%
	data.frame() %>%
	`[[`(1)

neutral.k <- spp.nmds.summary.DF %>%
	filter(Loci=='neutral') %>%
	select(k) %>%
	data.frame() %>%
	`[[`(1)

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
		if (ncol(curr.spp.coords.LST[[3]])>0) {
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
correlation.model.summary.DF <- ldply(
	seq_along(unique(spp.samples.sub.DF$species)),
	function(i) {
		if (!is.null(correlation.LST[[i]][[1]])) {
			env.ANOVA <- as.data.frame(anova(correlation.LST[[i]][[1]][[1]], correlation.LST[[i]][[1]][[2]]))
			env.df <- paste0(env.ANOVA$Df[[1]], ', ', env.ANOVA$Df[[2]])
			env.r2 <- r.squaredGLMM(correlation.LST[[i]][[1]][[1]])
		} else {
			env.ANOVA <- data.frame(F=c(NA, NA), Df=c(NA, NA), Chisq=c(NA,NA), P=c(NA, NA))
			names(env.ANOVA)[4]='Pr(>Chisq)'
			env.df <- NA
			env.r2 <- c(NA, NA)
		}
		geo.ANOVA <- as.data.frame(anova(correlation.LST[[i]][[2]][[1]], correlation.LST[[i]][[2]][[2]]))
		geo.df=paste0(geo.ANOVA$Df[[1]], ', ', geo.ANOVA$Df[[2]])
		geo.r2 <- r.squaredGLMM(correlation.LST[[i]][[2]][[1]])
		data.frame(
			Species=paste0('\\textit{',gsub('\\_', '\\ ', unique(spp.samples.sub.DF$species)[i]),'}'),
			
			Environmental.Marginal.R2=env.r2[1],
			Environmental.Conditional.R2=env.r2[2],
			Environmental.Chisq=env.ANOVA$Chisq[[2]],
			Environmental.raw.P=env.ANOVA[['Pr(>Chisq)']][[2]],
			Environmental.corr.P=NA,

			Geographic.Marginal.R2=geo.r2[1],
			Geographic.Conditional.R2=geo.r2[2],
			Geographic.Chisq=geo.ANOVA$Chisq[[2]],
			Geographic.raw.P=geo.ANOVA[['Pr(>Chisq)']][[2]],
			Geographic.corr.P=NA
		)
	}
) 
corr.pvalues <- p.adjust(c(correlation.model.summary.DF$Environmental.raw.P, correlation.model.summary.DF$Geographic.raw.P), 'bonferroni')

correlation.model.summary.DF <- correlation.model.summary.DF %>%
	mutate(Environmental.corr.P=corr.pvalues[seq_len(nrow(correlation.model.summary.DF))],
	Geographic.corr.P=corr.pvalues[nrow(correlation.model.summary.DF)+seq_len(nrow(correlation.model.summary.DF))])

### surrogacy analyses
## full model analysis
# run models
env.surrogacy.GLM <- glm(genetic.held~surrogate.held*Species, data=env.surrogacy.DF, family='binomial')
geo.surrogacy.GLM <- glm(genetic.held~surrogate.held*Species, data=geo.surrogacy.DF, family='binomial')

# assess term significance
env.surrogacy.AOV <- anova(env.surrogacy.GLM, test='Chisq') %>% tidy()
geo.surrogacy.AOV <- anova(geo.surrogacy.GLM, test='Chisq') %>% tidy()

## species-level analysis
# assess slope significance for each species
env.surrogacy.spp.slopes.DF <- tidy(summary(glht(env.surrogacy.GLM, linfct=c('surrogate.held == 0', paste0('surrogate.held + surrogate.held:Species',unique(env.surrogacy.DF$Species)[-1],' == 0'))), adjusted('bonferroni')))
geo.surrogacy.spp.slopes.DF <- tidy(summary(glht(geo.surrogacy.GLM, linfct=c('surrogate.held == 0', paste0('surrogate.held + surrogate.held:Species',unique(geo.surrogacy.DF$Species)[-1],' == 0'))), adjusted('bonferroni')))

# compute R^2 values for each species
env.surrogacy.LST <- list()
for (x in unique(env.surrogacy.DF$Species)) {
	curr.model <- glm(genetic.held~surrogate.held, data=filter(env.surrogacy.DF, Species==x), family='binomial')
	env.surrogacy.LST[[x]] <- list(model=curr.model, r2=pR2(curr.model))
}
geo.surrogacy.LST <- list()
for (x in unique(geo.surrogacy.DF$Species)) {
	curr.model <- glm(genetic.held~surrogate.held, data=filter(geo.surrogacy.DF, Species==x), family='binomial')
	geo.surrogacy.LST[[x]] <- list(model=curr.model, r2=pR2(curr.model))
}

env.surrogate.spp.DF <- cbind(
	data.frame(Species=unique(env.surrogacy.DF$Species)),
	select(env.surrogacy.spp.slopes.DF, -lhs, -rhs),
	select(ldply(env.surrogacy.LST, function(x) x$r2['r2CU']), -.id))

geo.surrogate.spp.DF <- cbind(
	data.frame(Species=unique(geo.surrogacy.DF$Species)),
	select(geo.surrogacy.spp.slopes.DF, -lhs, -rhs),
	select(ldply(geo.surrogacy.LST, function(x) x$r2['r2CU']), -.id))

env.surrogate.spp.DF2 <- env.surrogate.spp.DF
names(env.surrogate.spp.DF2)[2:6] <- paste0('Environmental.', names(env.surrogate.spp.DF)[2:6])
geo.surrogate.spp.DF2 <- geo.surrogate.spp.DF
names(geo.surrogate.spp.DF2)[2:6] <- paste0('Geographic.', names(geo.surrogate.spp.DF)[2:6])

surrogate.spp.DF <- full_join(env.surrogate.spp.DF2, geo.surrogate.spp.DF2, by='Species') %>%
	arrange(Species) %>% 
	mutate(
		Environmental.p.value=pvalString(Environmental.p.value),
		Geographic.p.value=pvalString(Geographic.p.value)
	)

# predict percentage of surrogate needed to secure a representative sample of genetic variation
env.surrogate.repr.prop.DF <- ldply(
	as.character(unique(env.surrogacy.DF$Species)),
	function(x) {
		curr.mod <- env.surrogacy.LST[[x]]$model
		curr.DF <- data.frame(surrogate.held=seq(0,1,length.out=10000))
		curr.DF$genetic.held <- predict(curr.mod, newdata=curr.DF, type='response')
		min.prop <- min(filter(curr.DF, genetic.held >= raptr.params.LST[[MODE]]$scenario.analysis$genetic.target)$surrogate.held)
		data.frame(Species=x, environmental.held=min.prop)
	}
)

geo.surrogate.repr.prop.DF <- ldply(
	as.character(unique(geo.surrogacy.DF$Species)),
	function(x) {
		curr.mod <- geo.surrogacy.LST[[x]]$model
		curr.DF <- data.frame(surrogate.held=seq(0,1,length.out=10000))
		curr.DF$genetic.held <- predict(curr.mod, newdata=curr.DF, type='response')
		if (max(curr.DF$genetic.held) < raptr.params.LST[[MODE]]$scenario.analysis$genetic.target) {
			min.prop <- 1
		} else {
			min.prop <- min(filter(curr.DF, genetic.held >= raptr.params.LST[[MODE]]$scenario.analysis$genetic.target)$surrogate.held)
		}
		data.frame(Species=x, geographic.held=min.prop)
	}
)

surrogate.repr.prop.DF <- merge(geo.surrogate.repr.prop.DF, env.surrogate.repr.prop.DF, by=c('Species'), all=TRUE)

### scenario analyses
## prepare data
# single species prioritisations without cost
single.spp.SDF <- single.spp.DF %>%
	gather(Metric, genetic.held, amount.held:neutral.held) %>%
	filter(Metric %in% c('adaptive.held', 'neutral.held')) %>%
	mutate(Context='single-species (equal costs)')

# single species prioritisations with cost
single.spp.with.cost.SDF <- single.spp.DF %>%
	gather(Metric, genetic.held, amount.held:neutral.held) %>%
	filter(Metric %in% c('adaptive.held', 'neutral.held')) %>%
	mutate(Context='single-species (opportunity costs)')

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
scenario.DF <- rbind.fill(list(single.spp.SDF, single.spp.with.cost.SDF, multi.spp.SDF, multi.spp.with.cost.SDF)) %>%
	mutate(Metric=revalue(Metric, c('adaptive.held'='Adaptive variation', 'neutral.held'='Neutral variation'))) %>%
	mutate(Context=revalue(Context, c(
		'single-species (equal costs)'='Single-species\n(equal costs)',
		'single-species (opportunity costs)'='Single-species\n(opportunity costs)',
		'multi-species (equal costs)'='Multi-species\n(equal costs)',
		'multi-species (opportunity costs)'='Multi-species\n(opportunity costs)'))) %>%
	mutate(Prioritisation=revalue(Prioritisation, c(
		'Amount'='Amount targets',
		'Surrogate'='Amount & surrogate\ntargets',
		'Genetic'='Amount & genetic\ntargets'
	))) %>%
	mutate(
		Metric=factor(as.character(Metric), levels=c('Adaptive variation','Neutral variation')),
		Context=factor(as.character(Context), levels=c('Single-species\n(equal costs)','Single-species\n(opportunity costs)','Multi-species\n(equal costs)','Multi-species\n(opportunity costs)')),
		Prioritisation=factor(as.character(Prioritisation), levels=c('Amount targets','Amount & surrogate\ntargets','Amount & genetic\ntargets'))
	) %>%
	mutate(Prioritisation.Metric.Context=interaction(Prioritisation,Metric,Context))
	
# remove NA values
scenario.sub.DF <- scenario.DF[rowSums(apply(select(scenario.DF, genetic.held, Prioritisation, Metric, Context), 2, is.na))==0,]

# run models
full.model <- glmer(genetic.held~Prioritisation*Metric*Context + (1|Species), data=scenario.sub.DF, family='binomial', nAGQ=10, control=glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=1e5)))
no.interaction.model <- glmer(genetic.held~Prioritisation+Metric+Context + (1|Species), data=scenario.sub.DF, family='binomial', nAGQ=10, control=glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=1e5)))
Context.model <- glmer(genetic.held~Context+(1|Species), data=scenario.sub.DF, family='binomial', nAGQ=10, control=glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=1e5)))
Metric.model <- glmer(genetic.held~Metric+(1|Species), data=scenario.sub.DF, family='binomial', nAGQ=10, control=glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=1e5)))
Prioritisation.model <- glmer(genetic.held~Prioritisation+(1|Species), data=scenario.sub.DF, family='binomial', nAGQ=10, control=glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=1e5)))
null.model <- glmer(genetic.held~1+(1|Species), data=scenario.sub.DF, family='binomial', nAGQ=10, control=glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=1e5)))

# generate anova tables
scenario.ANOVA.1.DF <- anova(full.model, no.interaction.model, test='Chisq') %>% tidy()
scenario.ANOVA.2.DF <- anova(Context.model, null.model, test='Chisq') %>% tidy()
scenario.ANOVA.3.DF <- anova(Metric.model, null.model, test='Chisq')  %>% tidy()
scenario.ANOVA.4.DF <- anova(Prioritisation.model, null.model, test='Chisq') %>% tidy()

# post-hoc analyses
scenario.GLHT.1.DF <- glht(Context.model, mcp(Context='Tukey')) %>% summary(adjusted('bonferroni')) %>% tidy()
scenario.GLHT.2.DF <- glht(Prioritisation.model, mcp(Prioritisation='Tukey')) %>% summary(adjusted('bonferroni')) %>% tidy()
scenario.GLHT.3.DF <- glht(Metric.model, mcp(Metric='Tukey')) %>% summary(adjusted('bonferroni')) %>% tidy()

## save .rda
save.session('data/intermediate/12-statistical-analysis.rda', compress='xz')
