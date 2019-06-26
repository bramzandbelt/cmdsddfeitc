# !/bin/bash
#
# 05_eda_grp.sh

nb_ix=5;
task=$1;

Rscript $PWD/analysis/run_script.R $nb_ix $task
