# !/bin/bash
#
# 03_computational_modeling_analysis_local_run_script.sh

# Parameters -------------------------------------------------------------------


for model_name in `echo $model_names`
do
  for parameterization in `echo $parameterizations`
  do
    echo "module unload R; module load R/3.5.1; source ~/.bashrc; cd /project/3017051.01; Rscript $PWD/analysis/run_script.R $nb_ix $pid $model_name $parameterization $bound_setting $comp_job" | qsub -N "comp_model_analysis_pid-$pid_model-$model_name_pmz-$parameterization" -l walltime=06:00:00,mem=1Gb;
  done
done

# Defer-speedup
excluded_pids="4 7 10 30 39 44 49 59 60 67 71";
pids="9 11 13 15 19 21 24 26 32 36 37 41 42 45 46 47 51 55 58 64 69 73";

# Date-delay
excluded_pids="3 12 18 22 29 31 40 48 53 54 61 70 72 76 78";
pids="1 6 14 17 20 23 25 27 33 35 38 43 50 52 56 62 65 68 74 75 77 79 81";

model_name="DFT_CRT";

parameterizations="time_scaling value_scaling time_scaling_t0 value_scaling_t0";

comp_job="vis";


for pid in `echo $pids`
  do
  for parameterization in `echo $parameterizations`
    do
      echo `cd ~/surfdrive/projects/cmdsddfeitc; sh analysis/03_computational_modeling_analysis_local.sh $pid $model_name $parameterization $comp_job`
    done
  done
