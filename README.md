<!-- README.md is generated from README.Rmd. Please edit that file -->
cmdsddfeitc - Cognitive Mechanisms of the Defer-Speedup and Date-Delay Framing Effects in InterTemporal Choice
==============================================================================================================

Overview
--------

The packagae `cmdsddfeitc` is a research compendium of the research project Cognitive Mechanisms of the Defer-Speedup and Date-Delay Framing Effects in Intertemporal Choice by Bram Zandbelt and Roshan Cools and was run at the Donders Institute, Radboud University / Radboucumc, Nijmegen, the Netherlands. This research project was funded through European Union’s Horizon 2020 research and innovation programme under the Marie Skłodowska-Curie grant agreement No. 703141. This research compendium is associated with a number of online objects, including the preregistration (<https://osf.io/rzqh9/>), data management plan (<https://doi.org/10.6084/m9.figshare.4720978>), cognitive task (<https://github.com/bramzandbelt/itch_time_framing_task>), and computational model (<https://github.com/bramzandbelt/itchmodel>).

<!-- ## Example -->
<!-- This is a basic example which shows you how to solve a common problem: -->
<!-- ```{r example} -->
<!-- ## basic example code -->
<!-- ``` -->
Getting started
---------------

### Organization

The following directory tree shows how the research compendium is organized:

    .
    ├── R                   # Project-specific R code
    ├── analysis            # R Markdown analysis notebook templates and bash files
    │   ├── 01_preprocessing.Rmd                    # Preprocesses the behavioral data (individual level)
    │   ├── 01_preprocessing.sh
    │   ├── 02_exploratory_data_analysis.Rmd        # EDA of choices and response times (individual level)
    │   ├── 02_exploratory_data_analysis.sh
    │   ├── 03_computational_modeling_analysis.Rmd  # Models of choices and response times and shows fit to the data (individual level)
    │   ├── 03_computational_modeling_analysis_cluster.sh
    │   ├── 03_computational_modeling_analysis_cluster_run_script.sh
    │   ├── 03_computational_modeling_analysis_cluster_tailored.sh
    │   ├── 03_computational_modeling_analysis_dftc_cluster.sh
    │   ├── 03_computational_modeling_analysis_local.sh
    │   ├── 03_computational_modeling_analysis_local_run_script.sh
    │   ├── 04_performance_descriptives_group.Rmd   
    │   ├── 04_sanity_check_control_trial_performance_group.Rmd   # Sanity checks on control trial performance (group-level)
    │   ├── 05_eda_overview.Rmd                                   # EDA of choices and response times (group level)
    │   ├── 06_model_comparison_group.Rmd                         # Identifies best-fitting model (group level)
    │   ├── 07_observed_vs_predicted_performance_group.Rmd        # Assesses model fit to the data (group level)
    │   ├── 08_analysis_of_model_parameters.Rmd
    │   ├── 09_sanity_check_effect_framing_on_model_predicted_auc.Rmd
    ├── data                # Data
    │   ├── derivatives     # - derivatives from raw data, organized by analysis
    │   ├── pilot           # - data from pilot
    │   └── raw             # - raw data
    ├── documents
    │   ├── information_for_participants
    │   ├── manuscript
    │   ├── miscellaneous
    │   └── presentations
    ├── figures             # Figure files, organized by analysis
    │   ├── 03_computational_modeling_analysis
    │   ├── 04_performance_descriptives_group
    │   ├── 04_sanity_check_control_trial_performance_group
    │   ├── 05_eda_overview_group
    │   ├── 05_model_comparison_group
    │   ├── 06_model_comparison_group
    │   ├── 07_observed_vs_predicted_performance_group
    │   ├── 08_analysis_of_model_parameters
    │   ├── 08_sanity_check_effect_framing_on_model_predicted_auc
    │   └── 09_sanity_check_effect_framing_on_model_predicted_auc
    ├── man
    ├── metadata            # Project's metadata
    │   └── raw             # - metadata describing content and context of raw data
    ├── opt
    │   └── IDP_R
    ├── packrat             # Packrat's directory containing dependencies
    └── reports             # Static HTML reports of analyses
        ├── 01_preprocessing
        ├── 02_exploratory_data_analysis
        └── 03_computational_modeling_analysis

### Usage

The notebooks are parameterized.

The following parameters can be set:

-   `participant_id`: the participant identifier (ranging from 1 to 93);
-   `task`: the task from which to process the data:
-   `defer_speedup`: intertemporal choice task with neutral, defer, and speedup frames;
-   `date_delay`: intertemporal choice task with delay and date frames;
-   `visualize`: whether or not to visualize the results (e.g. set to `FALSE` when fitting the model to the data);
-   `optimize`: whether or not to optimize the parameter values (i.e. set to `FALSE` when only visualizing the data);
-   `pars_from_file`: whether to use parameter values stored on disk (i.e. set to `TRUE` when visualizing the data)
-   `algorithm`: specification of the optimization algorithm:
-   `DEoptim`: differential evolution algorithm with (lower and upper) bound constraints;
-   `DEoptimR`: differential evolution algorithm with (lower and upper) bound and nonlinear constraints (i.e. ensuring that P(LL choice|SS amount = 0) &gt; 0.75 and P(LL choice| SS amount = LL amount) &lt; 0.25);
-   `model_name`: name of the model class to fit to the data, can be any of the following:
-   `DDM`: Drift-diffusion model (fits choices and response times);
-   `DFT_CRT`: Decision field theory (fits choices and response times);
-   `DFT_C`: Decision field theory (fits choices only);
-   `parameterization`: name of the parameterization to fit to the data:
-   `time_scaling`: lets `\kappa` parameter of the time function vary between frames;
-   `value_scaling`: lets `\mu` parameter of the value function vary between frames;
-   `time_and_value_scaling`: lets `\kappa` parameter of the time function and `\mu` parameter of the value function vary between frames;
-   `time_scaling_t0`: lets `\kappa` parameter of the time function and non-decision time (`t0`) vary between frames;
-   `value_scaling_t0`: lets `\mu` parameter of the value function and non-decision time (`t0`) vary between frames;
-   `time_and_value_scaling_t0`: lets `\kappa` parameter of the time function, `\mu` parameter of the value function, and non-decision time (`t0`) vary between frames;
-   `bound_setting`: controls the lower and upper bounds on the model parameters:
-   `standard`: uses parameter bounds specified in the preregistration document;
-   `wide`: uses wider parameter bounds, allowing parameters values to account for 'reverse' framing effects (e.g. more LL choices for delay than date frames);
-   `max_iter`: maximum iterations performed before optimization algorithm stops;
-   `rel_tol`: tolerance of the optimization algorithm's stopping criterion;
-   `n_pop_per_free_param`: number of population members per free parameter (see `NP` in `DEoptimR::JDEoptim` and `DEoptim::DEoptim.control`).

These parameter are used in the following analysis notebooks (indicated by their number only)

| parameter              | 01  | 02  | 03  | 04  | 05  | 06  | 07  | 08  | 09  |
|------------------------|-----|-----|-----|-----|-----|-----|-----|-----|-----|
| `participant_id`       | X   | X   | X   |     |     |     |     |     |     |
| `task`                 |     |     |     | X   | X   | X   | X   | X   | X   |
| `visualize`            | X   | X   | X   |     |     |     |     |     |     |
| `optimize`             |     |     | X   |     |     |     |     |     |     |
| `pars_from_file`       |     |     | X   |     |     |     |     |     |     |
| `algorithm`            |     |     | X   |     |     | X   | X   | X   | X   |
| `model_name`           |     |     | X   |     |     |     |     |     |     |
| `parameterization`     |     |     | X   |     |     |     |     |     |     |
| `bound_setting`        |     |     | X   |     |     |     |     |     |     |
| `max_iter`             |     |     | X   |     |     |     |     |     |     |
| `rel_tol`              |     |     | X   |     |     |     |     |     |     |
| `n_pop_per_free_param` |     |     | X   |     |     |     |     |     |     |

Reproducing the analyses
========================

Meaning of the notebook parameters
----------------------------------

-   by setting `graphics` to FALSE, the notebooks can be rendered on compute nodes that

A few notes:

-   Model fitting was done on the high performance cluster at the Donders Institute.

Colophon
--------

### Version

### Contact

([mailto:bramzandbelt@gmail.com](mailto:bramzandbelt@gmail.com))\[<bramzandbelt@gmail.com>\]

### References
