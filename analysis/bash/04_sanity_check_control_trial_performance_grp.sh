# !/bin/bash
#
# 04_sanity_check_control_trial_performance_grp.sh

nb_ix=4;
task=$1;

Rscript $PWD/analysis/run_script.R $nb_ix $task
