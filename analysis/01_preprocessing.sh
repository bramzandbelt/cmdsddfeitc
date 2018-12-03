# !/bin/bash
#
# 01_preprocessing.sh

nb_ix=1;
pid=$1;

Rscript $PWD/analysis/run_script.R $nb_ix $pid
