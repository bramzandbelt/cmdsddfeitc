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
  bound_setting <- args[5]
  comp_job <- args[6]

  if (comp_job == "all") {
    do_optimize = TRUE
    do_visualize = TRUE
  } else if (comp_job == "fit") {
    do_optimize = TRUE
    do_visualize = FALSE
  } else if (comp_job == "vis") {
    do_optimize = FALSE
    do_visualize = TRUE

    # Load best-fitting parameters from file

    data_dir <- NA


    par_vals_file <-
      file.path(data_output_dir,
                sprintf("best_fitting_params_task-.*_pid-%.3d_model-%s_pmz-%s_bounds-%s_BIC-%.0f.csv",
                        ,
                        pid, # pid
                        model_name, # mode
                        parameterization, # pmz
                        bound_setting, # bounds
                        optim_stats$BIC # BIC
                )
      )


  } else {
    do_optimize = TRUE
    do_visualize = TRUE
  }

  cmdsddfeitc::render_notebook(notebook_file = "03_computational_modeling_analysis.Rmd",
                               notebook_dir = "analysis",
                               reports_dir = "reports/03_computational_modeling_analysis",
                               params_tibble = tidyr::crossing(participant_id = pid,
                                                               model_name = model_name,
                                                               parameterization = parameterization,
                                                               bound_setting = "wide",
                                                               algorithm = "DEoptimR",
                                                               max_iter = 10,
                                                               rel_tol = 0.000001,
                                                               n_pop_per_free_param = 10,
                                                               optimize = do_optimize,
                                                               visualize = do_visualize),
                               force = TRUE)
}


