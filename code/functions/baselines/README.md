# Description 

This folder contains R code for baseline tests (iSKAT and GKM) from Maity and Lin (2011). 
The codebase is suitably extended to have the capacity of running kernel ensemble models (i.e., CVEK), although this ensemble implemented is not used in the experiments and is absorbed into the improved and better documented implementation under `./code/funcitons/cvek/.`


File structure:

* `README.md`		The readme file.
* `KMtest.R`		Functions to conduct kernel tests.
* `KMreg_null.R`	Functions to estimate the main-effect only kernel model.
* `./regression` 	Core functionalities for estimating kernel regression models.
* `./ensemble`		Core functionalities for running ensemble kernel models.

