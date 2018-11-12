# 01. Preprocessing

participant_id <- seq(36)

cmdsddfeitc::render_notebook(notebook_file = "01_preprocessing.Rmd",
                             notebook_dir = "analysis",
                             reports_dir = "reports/01_preprocessing",
                             params_tibble = tidyr::crossing(participant_id = participant_id),
                             force = TRUE)

# 02. Exploratory Data Analysis

participant_id <- c(1, 3, 4, 6, 7, 9, 10, 11, 12, 13, 14, 15, 17, 18, 19, 20,
                    21, 22, 23, 24, 25, 26, 27, 29, 30, 31, 32, 33, 35, 36)

cmdsddfeitc::render_notebook(notebook_file = "02_exploratory_data_analysis.Rmd",
                             notebook_dir = "analysis",
                             reports_dir = "reports/02_exploratory_data_analysis",
                             params_tibble = tidyr::crossing(participant_id = participant_id),
                             force = TRUE)

# 03. Computational modeling

# Defer-speedup
participant_id <- c(4,7,9,10,11,13,15,19,21,24,26,30,32,36,37,39,41,42)
# Date-delay
participant_id <- c(1,3,6,12,14,17,18,20,22,23,25,27,29,31,33,35,38,40,43)

pmzs <- c("time_scaling", "value_scaling")

cmdsddfeitc::render_notebook(notebook_file = "03_computational_modeling_analysis.Rmd",
                             notebook_dir = "analysis",
                             reports_dir = "reports/03_computational_modeling_analysis",
                             params_tibble = tidyr::crossing(participant_id = participant_id,
                                                             model_name = "DDM",
                                                             parameterization = pmzs,
                                                             bound_setting = "wide",
                                                             max_iter = 2500),
                             force = TRUE)
