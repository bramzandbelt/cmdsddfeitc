# !/bin/bash
#
# 07_observed_vs_predicted_performance_grp.sh

nb_ix=7;
task=$1;
algorithm=$2;

Rscript $PWD/analysis/run_script.R $nb_ix $task $algorithm
