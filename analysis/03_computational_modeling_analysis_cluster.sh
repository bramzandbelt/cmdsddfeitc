# !/bin/bash
#
# 03_computational_modeling_analysis_cluster.sh

# Parameters -------------------------------------------------------------------

# Notebook index
nb_ix=3;

# Participants
pid=$1;

# Model
model_names="DDM DFT_CRT";

# Parameterization
# parameterizations="time_scaling value_scaling time_scaling_t0 value_scaling_t0";
parameterizations="time_and_value_scaling time_and_value_scaling_t0";

# Maximum number of iterations
max_iter=15000;

# Bound settings
bound_setting="wide";

# Computation job: fit model ("fit"), visualize ("vis"), or both ("all")
comp_job="fit";

# Algorithm
algorithm="DEoptimR";

for model_name in `echo $model_names`
do
  for parameterization in `echo $parameterizations`
  do
    echo "module unload R; module load R/3.5.1; source ~/.bashrc; cd /project/3017051.01; Rscript $PWD/analysis/run_script.R $nb_ix $pid $model_name $parameterization $max_iter $bound_setting $comp_job $algorithm" | qsub -N "comp_model_analysis_pid-$pid-model-$model_name-pmz-$parameterization-max_iter-$max_iter" -l walltime=02:30:00,mem=1Gb;
  done
done

