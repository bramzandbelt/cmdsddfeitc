# !/bin/bash
#
# run_all_analyses.sh

# Define some general variables ================================================

model_names="DDM DFT_CRT";
pmzs="time_scaling value_scaling time_scaling_t0 value_scaling_t0 time_and_value_scaling time_and_value_scaling_t0";
algorithm="DEoptimR";
tasks="defer_speedup date_delay";

# Run individual-level analyses ================================================

# Step 1: preprocess all collected datasets
for pid in {1..93};
  do
    sh analysis/bash/01_preprocessing.sh $pid;
  done

# Step 2: identify unique participant IDs in directory with converged data files
project_dir=$PWD
preproc_data_dir=$project_dir/data/derivatives/01_preprocessing/included
cd $preproc_data_dir
pids_included=$(ls calibration_indifference_points_pid*.csv | grep -o -E '[0-9]+');
cd  $project_dir

# Step 3: do exploratory data analysis for included datasets

for pid in `echo $pids_included`;
  do
    sh analysis/bash/02_exploratory_data_analysis.sh $pid
  done

# Step 4: do computational modeling of all datasets (IDEALLY DONE IN PARALLEL ON A COMPUTER CLUSTER)



# Step 5: visualize individual model fits
comp_job="vis";

for model_name in `echo $model_names`;
  do
    for pmz in `echo $pmzs`;
    do
      for pid in `echo $pids_included`;
        do
          sh analysis/bash/03_computational_modeling_analysis_local.sh $pid $model_name $pmz $comp_job $algorithm
        done
    done
  done

# Run group-level analyses =====================================================

# Sanity check: control trial performance --------------------------------------

for task in `echo $tasks`;
  do
    sh analysis/bash/04_sanity_check_control_trial_performance_grp.sh $task
  done

# Exploratory data analysis of choices and response times ----------------------
for task in `echo $tasks`;
  do
    sh analysis/bash/05_eda_grp.sh $task
  done

# Model comparison based on the Bayesian Information Criterion -----------------
for task in `echo $tasks`;
  do
    sh analysis/bash/06_model_comparison_grp.sh $task $algorithm
  done

# Comparison of observed and predicted choices and response times --------------
for task in `echo $tasks`;
  do
    sh analysis/bash/07_observed_vs_predicted_performance_grp.sh $task $algorithm
  done

# Analysis of model parameters - distributions and relationships ---------------
for task in `echo $tasks`;
  do
    sh analysis/bash/08_analysis_of_model_parameters_grp.sh $task $algorithm
  done

# Sanity check: does best-fitting model replicate framing effect? --------------
for task in `echo $tasks`;
  do
    sh analysis/bash/09_sanity_check_effect_framing_on_model_predicted_auc_grp.sh $task $algorithm
  done
