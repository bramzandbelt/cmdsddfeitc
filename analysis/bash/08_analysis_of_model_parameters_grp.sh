# !/bin/bash
#
# 08_analysis_of_model_parameters_grp.sh

nb_ix=8;
task=$1;
algorithm=$2;

Rscript $PWD/analysis/run_script.R $nb_ix $task $algorithm
