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

# Bound setting
parameterization=$4

# Computation job: fit model ("fit"), visualize ("vis"), or both ("all")
comp_job=$5

Rscript $PWD/analysis/run_script.R $nb_ix $pid $model_name $parameterization $comp_job
