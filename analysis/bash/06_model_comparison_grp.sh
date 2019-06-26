# !/bin/bash
#
# 06_model_comparison_grp.sh

nb_ix=6;
task=$1;
algorithm=$2;

Rscript $PWD/analysis/run_script.R $nb_ix $task $algorithm
