# !/bin/bash
#
# 02_exploratory_data_analysis.sh

nb_ix=2;
pid=$1;

Rscript $PWD/analysis/run_script.R $nb_ix $pid
