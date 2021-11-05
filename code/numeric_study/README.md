# Reproducibility Instruction for Numeric Study.

This folder contains R code and shell scripts to run simulations and generate Figures 1 and C.1-C.4.


## Command line: 
1. Start terminal and set working directory to the `numeric_study` folder. (e.g., `cd ./CVEK-simulation`).
2. Under the `numeric_study` folder, run the shell scripts `command_exec1.sh` to `command_exec6.sh` separately (The scripts will submit many parallel jobs to run different sub-parts of the simulation, so it may take considerable time). Simulation results will be written to a file `./power_res.txt`.
3. After all jobs are completed, set working directory to the `./plot` folder and run `./plot_power.R` to generate subfigures of Figure 1 and Figure C.1-C.4. (stored under `./plot`, e.g., `Rscript ./plot_power.R`)


## Appendix: 

File structure of `numeric_study`.

README.md		The readme file.
main.R           	Main script to execute simulation runs in batch.
main_header.R		Utility functions for main script.
settings.txt            File that contains all simulation settings.
command_exec*.sh	6 shell scripts used to submit the simulation jobs to cluster. 
plot_power.R          	Script to plot Figures 1 and C.1-C.4. from simulation result.
./plot 		        Empty folder with subfoloders to write Figures 1 and C.1-C.4. to.

