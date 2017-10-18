## Environmental and geographic variables are effective surrogates for genetic variation in conservation planning

[![Status](https://img.shields.io/badge/status-in%20press-brightgreen.svg?style=flat-square)]()
[![License (GPL version 3)](https://img.shields.io/badge/license-GNU%20GPL%20version%203-brightgreen.svg?style=flat-square)](http://opensource.org/licenses/GPL-3.0)

[Jeffrey O. Hanson](wwww.jeffrey-hanson.com), Jonathan R. Rhodes, Cynthia Riginos, Richard A. Fuller

Correspondence should be addressed to [jeffrey.hanson@uqconnect.edu.au](mailto:jeffrey.hanson@uqconnect.edu.au)

### Overview

This repository contains the data and source code that underpins the findings in our manuscript "Environmental and geographic variables are effective surrogates for genetic variation in conservation planning_". [Download our data, code, results here](https://doi.org/10.5281/zenodo.843625). Alternatively, clone this repository, and rerun the entire analysis on your own computer using the system command `make all`.

To rerun all computational analyses, run `make clean && make all`.

* article
	+ manuscript main text, figures, and supporting information
* code
	+ [_R_](https://www.r-project.org): scripts used to run the analysis
	+ _parameters_: files used to run analysis in [TOML format](https://github.com/toml-lang/toml)
	+ [_rmarkdown_](http://rmarkdown.rstudio.com) files used to compile them manuscript
* data
	+ _raw_: raw data used to run the analysis
	+ _intermediate_: results generated during processing
	+ _final_: results used in the paper
* packrat
	+ _R_ packages used for analysis

### Abstract

Protected areas buffer species from anthropogenic threats and provide places for the processes that generate and maintain biodiversity to continue. However, genetic variation, the raw material for evolution, is difficult to capture in conservation planning, not least because genetic data require considerable resources to obtain and analyze. Here we show that freely available environmental and geographic distance variables can be highly effective surrogates in conservation planning for representing adaptive and neutral intra-specific genetic variation. We obtained occurrence and genetic data from the IntraBioDiv project for 27 plant species collected over the European Alps using a gridded sampling scheme. For each species, we identified loci that were potentially under selection using outlier loci methods, and mapped their main gradients of adaptive and neutral genetic variation across the grid cells. We then used the cells as planning units to prioritize protected area acquisitions. First, we verified that the spatial patterns of environmental and geographic variation were correlated, respectively, with adaptive and neutral genetic variation. Second, we showed that these surrogates can predict the proportion of genetic variation secured in randomly generated solutions. Finally, we discovered that solutions based only on surrogate information secured substantial amounts of adaptive and neutral genetic variation. Our work paves the way for widespread integration of surrogates for genetic variation into conservation planning.

### Software required

* Operating system
	+ Ubuntu (Trusty 14.04 LTS)
* Programs
	+ [R (version 3.3.2)](https://www.r-project.org)
	+ GNU make
	+ [pandoc (version 1.16.0.2+)](https://github.com/jgm/pandoc/releases)
	+ [Gurobi (version 7.0.2; academic licenses are available for no cost)](http://www.gurobi.com/)
	+ LaTeX
