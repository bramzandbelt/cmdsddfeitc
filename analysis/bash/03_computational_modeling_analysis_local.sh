# !/bin/bash
#
# 03_computational_modeling_analysis_local.sh

# Notebook index
nb_ix=3;

# Participant id
pid=$1;

# Model
model_name=$2

# Parameterization
parameterization=$3

# Max iter
max_iter=5000;

# Bound setting
bound_setting="wide";

# Computation job: fit model ("fit"), visualize ("vis"), or both ("all")
comp_job=$4

# Algorithm
algorithm="DEoptimR"

Rscript $PWD/analysis/run_script.R $nb_ix $pid $model_name $parameterization $max_iter $bound_setting $comp_job $algorithm
