# !/bin/bash
#
# 09_sanity_check_effect_framing_on_model_predicted_auc_grp.sh

nb_ix=9;
task=$1;
algorithm=$2;

Rscript $PWD/analysis/run_script.R $nb_ix $task $algorithm
