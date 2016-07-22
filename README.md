Are environmental and geographic effective surrogates for genetic variation in conservation planning?
========================================================================================================

[Jeffrey O. Hanson](wwww.jeffrey-hanson.com), Jonathan R. Rhodes, Cynthia Riginos, Hugh P. Possingham, Richard A. Fuller

Correspondance should be addressed to [jeffrey.hanson@uqconnect.edu.au](mailto:jeffrey.hanson@uqconnect.edu.au)

Source code for the manuscript entitled "_Are environmental and geographic effective surrogates for genetic variation in conservation planning?_". 

To rerun all computational analyses, run `make clean && make all`.

### Repository overview

* article
	+ manuscript main text, figures and tables
* data
	+ _raw_: raw data used to run the analysis
	+ _intermediate_: results generated during processing (eg. BayeScan output files)
	+ _final_: results used in the paper
* code
	+ [_R_](www.r-project.org): scripts used to run the analysis 
	+ _parameters_: files used to run analysis in [TOML format](https://github.com/toml-lang/toml)
	+ [_rmarkdown_](wwww.rmarkdown.rstudio.com) files used to compile them manuscript

### Software required

* Operating system
	+ Ubuntu (Trusty 14.04 LTS)
* Programs
	+ R (version 3.2.3)
	+ GNU make
	+ pandoc
	+ pandoc-citeproc
	+ LaTeX
