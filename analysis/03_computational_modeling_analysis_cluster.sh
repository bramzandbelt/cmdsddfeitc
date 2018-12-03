# !/bin/bash
#
# 03_computational_modeling_analysis_cluster.sh

# Make sure we have the right version of R
module unload R
module load R/3.5.1

# Make sure that pandoc is accessible from R
source ~/.bashrc

# Go to the project directory
cd /project/3017051.01

# Parameters -------------------------------------------------------------------

# Notebook index
nb_ix=3

# Participants
pids=$1;

# Model
model_names="DDM"

# Parameterization
parameterizations="one_condition time_scaling value_scaling time_scaling_t0 value_scaling_t0"

# Bound settings
bound_setting="wide"

# Computation job: fit model ("fit"), visualize ("vis"), or both ("all")
comp_job="fit"

for model_name in `echo $model_names`
do
  for parameterization in `echo $parameterizations`
  do
    echo "Rscript $PWD/analysis/run_script.R $nb_ix $pid $model_name $parameterization $bound_setting $comp_job" | qsub -N "comp_model_analysis_pid-$pid_model-$model_name_pmz-$parameterization" -l walltime=06:00:00,mem=1Gb;
  done
done
