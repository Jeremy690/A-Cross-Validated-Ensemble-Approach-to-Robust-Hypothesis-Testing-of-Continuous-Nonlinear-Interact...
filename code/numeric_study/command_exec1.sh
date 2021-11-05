#!/bin/bash
#SBATCH --array=1-1000	    		#job array list goes 1,2,3...n
#SBATCH -c 1
#SBATCH -N 1
#SBATCH -t 0-02:00			#job run for up to 2 hours
#SBATCH -p short
#SBATCH --mem=500  		# use at most 500MB memory
#SBATCH -o %A_%a.out
#SBATCH -e %A_%a.err
#SBATCH --mail-type=END      #Type of email notification
#SBATCH --mail-user=wdeng@g.harvard.edu
Rscript './main.R' ${SLURM_ARRAY_TASK_ID}
