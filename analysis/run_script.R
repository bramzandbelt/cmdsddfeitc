require(tidyverse)
require(DT)
library(cmdsddfeitc)

# Provides access to a copy of the command line arguments, which specifies what to do
args <- commandArgs(TRUE)

# Get index of notebook to render to identify notebook filename
notebook_ix <- as.integer(args[1])

notebook_file <-
  switch(notebook_ix,
         "1" = "01_preprocessing_idv.Rmd",
         "2" = "02_eda_idv.Rmd",
         "3" = "03_computational_modeling_analysis_idv.Rmd",
         "4" = "04_sanity_check_control_trial_performance_grp.Rmd",
         "5" = "05_eda_grp.Rmd",
         "6" = "06_model_comparison_grp.Rmd",
         "7" = "07_observed_vs_predicted_performance_grp.Rmd",
         "8" = "08_analysis_of_model_parameters_grp.Rmd",
         "9" = "09_sanity_check_effect_framing_on_model_predicted_auc_grp.Rmd"
         )

# Define input dir (notebook_templates_dir) and outpur dir (reports_dir)
notebook_templates_dir <- "analysis/notebook_templates"
reports_dir <- file.path("reports", stringr::str_replace(notebook_file,
                                                         pattern = ".Rmd",
                                                         replacement = ""))

# Specify notebook parameters based on notebook index --------------------------

if (notebook_ix == 1) {

  params_tibble <-
    tidyr::crossing(participant_id = as.integer(args[2]),
                    visualize = TRUE)

} else if (notebook_ix == 2) {
  params_tibble <-
    tidyr::crossing(participant_id = as.integer(args[2]),
                    visualize = TRUE)
} else if (notebook_ix == 3) {

  pid <- as.integer(args[2])
  model_name <- args[3]
  parameterization <- args[4]
  max_iter <- as.integer(args[5])
  bound_setting <- args[6]
  comp_job <- args[7]
  algorithm <- args[8]

  print(sprintf("Participant id: %d", pid))
  print(sprintf("Model name: %s", model_name))
  print(sprintf("Parameterization: %s", parameterization))
  print(sprintf("Max. number of iterations: %d", max_iter))
  print(sprintf("Bound setting: %s", bound_setting))
  print(sprintf("Computing job: %s", comp_job))
  print(sprintf("Optimization algorithm: %s", algorithm))

  if (comp_job == "all") {
    do_optimize = TRUE
    do_visualize = TRUE
  } else if (comp_job == "fit") {
    do_optimize = TRUE
    do_visualize = FALSE
  } else if (comp_job == "vis") {
    do_optimize = FALSE
    do_visualize = TRUE
  } else {
    do_optimize = TRUE
    do_visualize = TRUE
  }

  if (comp_job == "vis") {
    params_tibble <-
      tidyr::crossing(participant_id = pid,
                      model_name = model_name,
                      parameterization = parameterization,
                      bound_setting = "wide",
                      algorithm = algorithm,
                      max_iter = max_iter,
                      rel_tol = 0.000001,
                      n_pop_per_free_param = 20,
                      optimize = do_optimize,
                      visualize = do_visualize,
                      pars_from_file = TRUE)
  } else {
    params_tibble <-
      tidyr::crossing(participant_id = pid,
                      model_name = model_name,
                      parameterization = parameterization,
                      bound_setting = "wide",
                      algorithm = algorithm,
                      max_iter = max_iter,
                      rel_tol = 0.000001,
                      n_pop_per_free_param = 20,
                      optimize = do_optimize,
                      visualize = do_visualize)
  }
} else if (notebook_ix == 4) {

  params_tibble <-
    tidyr::crossing(task = args[2])

} else if (notebook_ix == 5) {

  params_tibble <-
    tidyr::crossing(task = args[2])

} else if (notebook_ix == 6) {

  params_tibble <-
    tidyr::crossing(task = args[2],
                    algorithm = args[3])

} else if (notebook_ix == 7) {

  params_tibble <-
    tidyr::crossing(task = args[2],
                    algorithm = args[3])

} else if (notebook_ix == 8) {

  params_tibble <-
    tidyr::crossing(task = args[2],
                    algorithm = args[3])

} else if (notebook_ix == 9) {

  params_tibble <-
    tidyr::crossing(task = args[2],
                    algorithm = args[3])

}

# Render notebook into static HTML file ----------------------------------------
cmdsddfeitc::render_notebook(notebook_file = notebook_file,
                             notebook_dir = notebook_templates_dir,
                             reports_dir = reports_dir,
                             params_tibble = params_tibble,
                             force = TRUE)

