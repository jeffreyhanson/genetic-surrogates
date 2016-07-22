parameters
==========

The files in this directory contain parameters used to run analyses. These files are in the [TOML format](https://github.com/toml-lang/toml). Each file contains a `debugging` set of parameters that are useful for finding errors, and a `release` set of parameters that were used for inference.

* adegenet.toml
	+ parameters used to split individuals into different populations
* bayescan.toml
	+ parameters used to identify loci underselection
* general.toml
	+ general parameters (eg. number of species to run).
* gurobi.toml
	+ parameters used to control the behaviour of the reserve selection optimization
* nmds.toml
	+ parameters used to identify the main themes of genetic variation for each species using NMDS
* raptr.toml
	+ parameters specifying targets used to generate prioritizations