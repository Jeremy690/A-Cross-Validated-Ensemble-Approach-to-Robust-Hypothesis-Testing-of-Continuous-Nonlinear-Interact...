# Reproducibility Instruction for Data Analysis.

This folder contains R code to run data analysis on the Bangledash data and generate Table 2.

## Command line: 
1. Start terminal and set working directory to the `./code/data_analysis` folder. (e.g., `cd ./code/data_analysis`).
2. Run analysis by executing the `data_analysis_main.R` script. e.g. `Rscript ./data_analysis_main.R`.
3. Result table will appear in the `./code/data_analysis/result/` directory.

## In R GUI / RStudio:
1. Start R GUI and set working directory to the `data_analysis` folder. (e.g., `setwd("~/PARENT_DIRECTORY/code/data_analysis/")`).
2. Execute the content of `data_analysis_main.R` script. (e.g., `source("./data_analysis_main.R")`)
3. Result table will appear in the `./code/data_analysis/result/` directory.

## Appendix: 

File structure of `./data_analysis`.

main.R 			Main script for executing data analysis.
main_header.R 		Utility functions for data analysis.
README.md		The readme file.
./data			Folder that contains the data file and the associated data dictionary.

