require(tidyverse)
library(cmdsddfeitc)

args <- commandArgs(TRUE)

notebook_ix <- as.integer(args[1])

notebook_file <- switch(notebook_ix,
                        "1" = "01_preprocessing.Rmd",
                        "2" = "02_exploratory_data_analysis.Rmd",
                        "3" = "03_computational_modeling_analysis.Rmd")


if (notebook_ix == 1) {
  # Participant ID
  pid <- as.integer(args[2])

  cmdsddfeitc::render_notebook(notebook_file = "01_preprocessing.Rmd",
                               notebook_dir = "analysis",
                               reports_dir = "reports/01_preprocessing",
                               params_tibble = tidyr::crossing(participant_id = pid,
                                                               visualize = TRUE),
                               force = TRUE)

} else if (notebook_ix == 2) {
  # Participant ID
  pid <- as.integer(args[2])

  cmdsddfeitc::render_notebook(notebook_file = "02_exploratory_data_analysis.Rmd",
                               notebook_dir = "analysis",
                               reports_dir = "reports/02_exploratory_data_analysis",
                               params_tibble = tidyr::crossing(participant_id = pid,
                                                               visualize = TRUE),
                               force = TRUE)

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
    cmdsddfeitc::render_notebook(notebook_file = "03_computational_modeling_analysis.Rmd",
                                 notebook_dir = "analysis",
                                 reports_dir = "reports/03_computational_modeling_analysis",
                                 params_tibble = tidyr::crossing(participant_id = pid,
                                                                 model_name = model_name,
                                                                 parameterization = parameterization,
                                                                 bound_setting = "wide",
                                                                 algorithm = algorithm,
                                                                 max_iter = max_iter,
                                                                 rel_tol = 0.000001,
                                                                 n_pop_per_free_param = 10,
                                                                 optimize = do_optimize,
                                                                 visualize = do_visualize,
                                                                 pars_from_file = TRUE),
                                 force = TRUE)
    } else {
      cmdsddfeitc::render_notebook(notebook_file = "03_computational_modeling_analysis.Rmd",
                                   notebook_dir = "analysis",
                                   reports_dir = "reports/03_computational_modeling_analysis",
                                   params_tibble = tidyr::crossing(participant_id = pid,
                                                                   model_name = model_name,
                                                                   parameterization = parameterization,
                                                                   bound_setting = "wide",
                                                                   algorithm = algorithm,
                                                                   max_iter = max_iter,
                                                                   rel_tol = 0.000001,
                                                                   n_pop_per_free_param = 10,
                                                                   optimize = do_optimize,
                                                                   visualize = do_visualize),
                                   force = TRUE)
      }
}


