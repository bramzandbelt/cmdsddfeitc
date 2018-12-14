# ==============================================================================

#' Get column types for extracting data from csv files
#'
#' File type can be any of the following:
#'
#' trial_data_ds - tidied trial-level data of defer-speedup task
#' trial_data_dd - tidied trial-level data of date-delay task
#'
#' @param file_type Type of file to be read
#' @export
get_col_types <- function(file_type){

  switch(
    tolower(file_type),
    auc_defer_speedup =
      readr::cols_only(participant_id =
                         readr::col_integer(),
                       model =
                         readr::col_factor(levels = c("DDM",
                                                      "DFT_C"),
                                           include_na = TRUE),
                       parameterization =
                         readr::col_factor(levels = c("one_condition",
                                                      "time_scaling",
                                                      "time_scaling_t0",
                                                      "value_scaling",
                                                      "value_scaling_t0"),
                                           include_na = TRUE),
                       bound_settings =
                         readr::col_factor(levels = c("standard",
                                                      "wide"),
                                           include_na = TRUE),
                       algorithm =
                         readr::col_factor(levels = c("DEoptimR",
                                                      "DEoptim"),
                                           include_na = TRUE),
                       frame =
                         readr::col_factor(levels = c("neutral",
                                                      "defer",
                                                      "speedup"),
                                           include_na = TRUE,
                                           ordered = TRUE),
                       auc =
                         readr::col_double()),
    auc_date_delay =
      readr::cols_only(participant_id =
                         readr::col_integer(),
                       model =
                         readr::col_factor(levels = c("DDM",
                                                      "DFT_C"),
                                           include_na = TRUE),
                       parameterization =
                         readr::col_factor(levels = c("one_condition",
                                                      "time_scaling",
                                                      "time_scaling_t0",
                                                      "value_scaling",
                                                      "value_scaling_t0"),
                                           include_na = TRUE),
                       bound_settings =
                         readr::col_factor(levels = c("standard",
                                                      "wide"),
                                           include_na = TRUE),
                       algorithm =
                         readr::col_factor(levels = c("DEoptimR",
                                                      "DEoptim"),
                                           include_na = TRUE),
                       frame =
                         readr::col_factor(levels = c("delay",
                                                      "date"),
                                           include_na = TRUE,
                                           ordered = TRUE),
                       auc =
                         readr::col_double()),
    best_fitting_params =
      readr::cols(participant_id =
                    readr::col_integer(),
                 model_name =
                   readr::col_factor(levels = c("DDM",
                                                "DFT_C"),
                                                include_na = TRUE),
                 parameterization =
                   readr::col_factor(levels = c("one_condition",
                                                "time_scaling",
                                                "time_scaling_t0",
                                                "value_scaling",
                                                "value_scaling_t0"),
                                     include_na = TRUE),
                 bound_settings =
                   readr::col_factor(levels = c("standard",
                                                "wide"),
                                     include_na = TRUE),
                 algorithm =
                   readr::col_factor(levels = c("DEoptimR",
                                                "DEoptim"),
                                     include_na = TRUE),
                 .default = readr::col_double()),
    calibration_indifference_points =
      readr::cols_only(subject_ix = readr::col_integer(),
                       t_l = readr::col_integer(),
                       ip = readr::col_double()),
    choice_percentages_defer_speedup =
      readr::cols_only(participant_id = readr::col_integer(),
                       frame = readr::col_factor(levels = c("neutral", "defer", "speedup"),
                                                 include_na = TRUE,
                                                 ordered = TRUE),
                       choice = readr::col_factor(levels = c("ss",
                                                                      "ll"),
                                                           include_na = TRUE,
                                                           ordered = TRUE),
                       n = readr::col_integer(),
                       percentage = readr::col_double()),
    choice_percentages_date_delay =
      readr::cols_only(participant_id = readr::col_integer(),
                       frame = readr::col_factor(levels = c("delay", "date"),
                                                 include_na = TRUE,
                                                 ordered = TRUE),
                       choice = readr::col_factor(levels = c("ss",
                                                                      "ll"),
                                                           include_na = TRUE,
                                                           ordered = TRUE),
                       n = readr::col_integer(),
                       percentage = readr::col_double()),
    expt_standard_trials_defer_speedup =
      readr::cols_only(subject_ix = readr::col_integer(),
                       frame = readr::col_factor(levels = c("neutral", "defer", "speedup"),
                                                   include_na = TRUE,
                                                   ordered = TRUE),
                       m_s_cat = readr::col_factor(levels = c("below_ip", "at_ip", "above_ip"),
                                                   include_na = TRUE,
                                                   ordered = FALSE),
                       m_s = readr::col_double(),
                       m_l = readr::col_double(),
                       t_s = readr::col_integer(),
                       t_l = readr::col_integer(),
                       choice = readr::col_factor(levels = c("ss",
                                                             "ll"),
                                                  include_na = TRUE,
                                                  ordered = TRUE),
                       response = readr::col_character(),
                       rt = readr::col_double()
                       ),
    expt_standard_trials_date_delay =
      readr::cols_only(subject_ix = readr::col_integer(),
                       frame = readr::col_factor(levels = c("delay", "date"),
                                                   include_na = TRUE,
                                                   ordered = TRUE),
                       m_s_cat = readr::col_factor(levels = c("below_ip", "at_ip", "above_ip"),
                                                   include_na = TRUE,
                                                   ordered = FALSE),
                       m_s = readr::col_double(),
                       m_l = readr::col_double(),
                       t_s = readr::col_integer(),
                       t_l = readr::col_integer(),
                       choice = readr::col_factor(levels = c("ss",
                                                             "ll"),
                                                  include_na = TRUE,
                                                  ordered = TRUE),
                       response = readr::col_character(),
                       rt = readr::col_double()),
    optim_stats_defer_speedup =
      readr::cols_only(participant_id = readr::col_integer(),
                       model =
                         readr::col_factor(levels = c("DDM",
                                                      "DFT_C"),
                                           include_na = TRUE),
                       parameterization =
                         readr::col_factor(levels = c("one_condition",
                                                      "defer_speedup_time_scaling",
                                                      "defer_speedup_time_scaling_t0",
                                                      "defer_speedup_value_scaling",
                                                      "defer_speedup_value_scaling_t0"),
                                           include_na = TRUE),
                       bound_settings =
                         readr::col_factor(levels = c("standard",
                                                      "wide"),
                                           include_na = TRUE),
                       algorithm =
                         readr::col_factor(levels = c("DEoptimR",
                                                      "DEoptim"),
                                           include_na = TRUE),
                       frame =
                         readr::col_factor(levels = c("neutral",
                                                      "defer",
                                                      "speedup"),
                                           include_na = TRUE,
                                           ordered = TRUE),
                       n_iter =
                         readr::col_integer(),
                       converged =
                         readr::col_logical(),
                       LL =
                         readr::col_double(),
                       AIC =
                         readr::col_double(),
                       BIC =
                         readr::col_double()
                       ),
    optim_stats_date_delay =
      readr::cols_only(participant_id = readr::col_integer(),
                       model =
                         readr::col_factor(levels = c("DDM",
                                                      "DFT_C"),
                                           include_na = TRUE),
                       parameterization =
                         readr::col_factor(levels = c("one_condition",
                                                      "date_delay_time_scaling",
                                                      "date_delay_time_scaling_t0",
                                                      "date_delay_value_scaling",
                                                      "date_delay_value_scaling_t0"),
                                           include_na = TRUE),
                       bound_settings =
                         readr::col_factor(levels = c("standard",
                                                      "wide"),
                                           include_na = TRUE),
                       algorithm =
                         readr::col_factor(levels = c("DEoptimR",
                                                      "DEoptim"),
                                           include_na = TRUE),
                       frame =
                         readr::col_factor(levels = c("delay",
                                                      "date"),
                                           include_na = TRUE,
                                           ordered = TRUE),
                       n_iter =
                         readr::col_integer(),
                       converged =
                         readr::col_logical(),
                       LL =
                         readr::col_double(),
                       AIC =
                         readr::col_double(),
                       BIC =
                         readr::col_double()
      ),
    predicted_ip_defer_speedup =
      readr::cols_only(participant_id =
                         readr::col_integer(),
                       model =
                         readr::col_factor(levels = c("DDM",
                                                      "DFT_C"),
                                           include_na = TRUE),
                       # TODO: Make sure that computational modeling notebook writes parmaterization as task + parameterization
                       parameterization =
                         readr::col_factor(levels = c("one_condition",
                                                      "time_scaling",
                                                      "time_scaling_t0",
                                                      "value_scaling",
                                                      "value_scaling_t0"),
                                           include_na = TRUE),
                       bound_settings =
                         readr::col_factor(levels = c("standard",
                                                      "wide"),
                                           include_na = TRUE),
                       algorithm =
                         readr::col_factor(levels = c("DEoptimR",
                                                      "DEoptim"),
                                           include_na = TRUE),
                       frame =
                         readr::col_factor(levels = c("neutral",
                                                      "defer",
                                                      "speedup"),
                                           include_na = TRUE,
                                           ordered = TRUE),
                       ip =
                         readr::col_double(),
                       delay =
                         readr::col_integer()
                       ),
    predicted_ip_date_delay =
      readr::cols_only(participant_id =
                         readr::col_integer(),
                       model =
                         readr::col_factor(levels = c("DDM",
                                                      "DFT_C"),
                                           include_na = TRUE),
                       # TODO: Make sure that computational modeling notebook writes parmaterization as task + parameterization
                       parameterization =
                         readr::col_factor(levels = c("one_condition",
                                                      "time_scaling",
                                                      "time_scaling_t0",
                                                      "value_scaling",
                                                      "value_scaling_t0"),
                                           include_na = TRUE),
                       bound_settings =
                         readr::col_factor(levels = c("standard",
                                                      "wide"),
                                           include_na = TRUE),
                       algorithm =
                         readr::col_factor(levels = c("DEoptimR",
                                                      "DEoptim"),
                                           include_na = TRUE),
                       frame =
                         readr::col_factor(levels = c("delay",
                                                      "date"),
                                           include_na = TRUE,
                                           ordered = TRUE),
                       ip =
                         readr::col_double(),
                       delay =
                         readr::col_integer()
      ),
      readr::cols_only(subject_ix = readr::col_integer()),
    trial_log_data_defer_speedup =
      readr::cols_only(subject_ix = readr::col_integer(),
                       block_id = readr::col_character(),
                       block_ix = readr::col_double(),
                       iter_ix = readr::col_double(),
                       framing = readr::col_factor(levels = c("neutral", "defer", "speedup"),
                                                   include_na = TRUE,
                                                   ordered = TRUE),
                       m_s = readr::col_double(),
                       m_s_cat = readr::col_factor(levels = c("below_ip", "at_ip", "above_ip"),
                                                   include_na = TRUE,
                                                   ordered = FALSE),
                       m_l = readr::col_double(),
                       t_s = readr::col_integer(),
                       t_l = readr::col_integer(),
                       trial_type = readr::col_factor(levels = c("standard",
                                                                 "catch_ss",
                                                                 "catch_ll",
                                                                 "instr_check"),
                                                      include_na = TRUE,
                                                      ordered = FALSE),
                       choice = readr::col_factor(levels = c("ss",
                                                             "ll"),
                                                  include_na = TRUE,
                                                  ordered = TRUE),
                       rt = readr::col_double(),
                       trial_correct = readr::col_logical(),
                       too_fast = readr::col_logical()
                       ),
    trial_log_data_date_delay =
      readr::cols_only(subject_ix = readr::col_integer(),
                       block_id = readr::col_character(),
                       block_ix = readr::col_double(),
                       iter_ix = readr::col_double(),
                       framing = readr::col_factor(levels = c("neutral", "delay", "date"),
                                                   include_na = TRUE,
                                                   ordered = TRUE),
                       m_s = readr::col_double(),
                       m_s_cat = readr::col_factor(levels = c("below_ip", "at_ip", "above_ip"),
                                                   include_na = TRUE,
                                                   ordered = FALSE),
                       m_l = readr::col_double(),
                       t_s = readr::col_integer(),
                       t_l = readr::col_integer(),
                       trial_type = readr::col_factor(levels = c("standard",
                                                                 "catch_ss",
                                                                 "catch_ll"),
                                                      include_na = TRUE,
                                                      ordered = FALSE),
                       choice = readr::col_factor(levels = c("ss",
                                                             "ll"),
                                                  include_na = TRUE,
                                                  ordered = TRUE),
                       rt = readr::col_double(),
                       trial_correct = readr::col_logical(),
                       too_fast = readr::col_logical()
      )
  )
}

## tidy_obs_prd_choice_rt #######################################################
#' Returns choices and response times observed and predicted for plotting in tidy, nested format
#'
#'
#' @param obs Tibble containing observed data
#' @param model Name of model (e.g. "DDM", "DFT_C", "DFT_RT")
#' @param pmz Parameterization (e.g. "date_delay_value_scaling)
#' @param parameters Vector (unnamed) with best-fitting parameter values
#' @export
tidy_obs_prd_choice_rt <- function(return_var="obs", obs, model="", pmz="", parameters=NA) {

  assertthat::assert_that(return_var %in% c("obs", "prd", "all"),
                          msg = "The value of return_var should be 'obs' (tidy observations only), 'prd' (tidy predictions only), or 'all' (tidy predictions and observations).")

  t_ls <- sort(unique(obs$t_l))
  t_s <- 0
  m_l <- 43.52
  m_s_min <- 0
  m_s_max <- m_l
  m_s_step <- 0.17

  # Tidy and group observed data
  obs_grouped <-
    obs %>%
    # Cleaning
    dplyr::rename(participant_id = subject_ix) %>%
    dplyr::mutate_if(is.numeric, round, 2) %>%
    # Arranging and grouping
    dplyr::arrange(t_l, m_s) %>%
    dplyr::group_by(participant_id, frame)

  stim_nested <-
    obs_grouped %>%
    dplyr::select(participant_id, frame, m_s_cat, m_s, t_s, m_l, t_l) %>%
    tidyr::nest(.key = "stimuli")

  resp_nested <-
    obs_grouped %>%
    dplyr::select(participant_id, frame, rt, response) %>%
    tidyr::nest(.key = "observations")

  sum_stats_nested <-
    obs_grouped %>%
    dplyr::ungroup() %>%
    dplyr::mutate(t_l = factor(t_l,
                               levels = t_ls,
                               labels = stringr::str_c(t_ls, " days"),
                               ordered = TRUE),
                  m_s_cat = factor(m_s_cat,
                                   labels = c("below IP", "at IP", "above IP"))) %>%
    dplyr::group_by(participant_id, frame, t_l, m_s_cat) %>%
    dplyr::summarize(m_s = median(m_s),
                     m_l = median(m_l),
                     t_s = median(t_s),
                     p_ll = sum((response == "upper")) / n(),
                     med_rt = median(rt)) %>%
    dplyr::ungroup() %>%
    dplyr::select(participant_id, frame, m_s_cat, m_s, t_s, m_l, t_l, p_ll, med_rt) %>%
    dplyr::group_by(participant_id, frame) %>%
    tidyr::nest(.key = "summary_stats")

  obs_nested <-
    dplyr::left_join(x = stim_nested,
                     y = resp_nested,
                     by = c("participant_id", "frame")
    ) %>%
    dplyr::left_join(sum_stats_nested,
                     by = c("participant_id", "frame"))

  # Now, make sure that order of frames is correct, then convert to character
  # (necessary for the modeling code to identiy identify the right data subset)
  obs_nested <-
    obs_nested %>%
    dplyr::arrange(frame) %>%
    dplyr::mutate(frame = as.character(frame))

  if (return_var %in% c("prd", "all")) {
    # Nest and tidy predicted data

    # Best-fitting parameters, per frame
    params_nested <-
      parameters %>%
      dplyr::select(-model_name, -parameterization, -bound_settings, -algorithm)

    par_names <-
      colnames(params_nested %>% dplyr::select(-participant_id))

    params_nested <-
      params_nested %>%
      tidyr::nest(par_names, .key = "parameters") %>%
      dplyr::mutate(params_per_frame = purrr::pmap(.l = list(x = .$parameters),
                                                   .f = function(x, parameterization, model) {
                                                     itchmodel::get_par_values(x,
                                                                               parameterization = parameterization,
                                                                               model = model) %>%
                                                       dplyr::bind_rows() %>%
                                                       dplyr::mutate(frame = factor(itchmodel::get_frames(parameterization = parameterization),
                                                                                    levels = itchmodel::get_frames(parameterization = parameterization),
                                                                                    ordered = TRUE)) %>%
                                                       dplyr::select(frame, dplyr::everything())


                                                   },
                                                   parameterization = pmz,
                                                   model = model)
      ) %>%
      dplyr::select(-parameters) %>%
      tidyr::unnest(params_per_frame) %>%
      tidyr::nest(-participant_id, -frame, .key = "parameters")

    # Make model predictions based on best-fitting parameters
    prd_nested <-
      tidyr::crossing(participant_id = unique(obs_nested$participant_id),
                      frame = factor(itchmodel::get_frames(parameterization = pmz),
                                     levels = itchmodel::get_frames(parameterization = pmz),
                                     ordered = TRUE),
                      t_l = t_ls,
                      t_s = t_s,
                      m_l = m_l,
                      m_s = seq(m_s_min, m_s_max, m_s_step)) %>%
      dplyr::group_by(participant_id, frame) %>%
      tidyr::nest(.key = "stimuli") %>%
      dplyr::left_join(params_nested %>%
                         dplyr::mutate(frame = factor(.$frame,
                                                      levels = itchmodel::get_frames(parameterization = pmz),
                                                      ordered = TRUE)),
                       by = c("participant_id", "frame")) %>%
      dplyr::mutate(du = purrr::pmap(.l = list(parameters = .$parameters,
                                               stimuli = .$stimuli,
                                               frame = as.character(.$frame)),
                                     .f = itchmodel::compute_transformation_diffs,
                                     parameterization = pmz,
                                     variable = "du"),
                    dp = purrr::pmap(.l = list(parameters = .$parameters,
                                               stimuli = .$stimuli,
                                               frame = as.character(.$frame)),
                                     .f = itchmodel::compute_transformation_diffs,
                                     parameterization = pmz,
                                     variable = "dp")) %>%
      dplyr::mutate(d = purrr::pmap(.l = list(parameters = .$parameters,
                                              stimuli = .$stimuli,
                                              frame = as.character(.$frame)),
                                    .f = itchmodel::compute_drift_rate,
                                    parameterization = parameterization)
      )

    if (model %in% c("DFT_C", "DFT_CRT")) {
      prd_nested <-
        prd_nested %>%
        dplyr::mutate(s = purrr::pmap(.l = list(x = .$parameters,
                                                du = .$du,
                                                dp = .$dp,
                                                d = .$d),
                                      .f = function(x, du, dp, d) {
                                        sqrt(as.double(x["w"]) * du^2 + (1 - as.double(x["w"])) * dp^2 - d^2)
                                      })
        ) %>%
        dplyr::mutate(p_ll = purrr::pmap(.l = list(d = .$d,
                                                   s = .$s,
                                                   parameters = .$parameters),
                                         .f = function(d, s, parameters) {
                                           theta_star <- as.double(parameters["theta_star"])

                                           itchmodel::dft_cp(d = d,
                                                             s = s,
                                                             theta = theta_star * s,
                                                             z = rep(0,length(d)),
                                                             response = rep("upper",length(d)))}
        )
        )


    } else if (model == "DDM") {
      prd_nested <-
        prd_nested %>%
        dplyr::mutate(p_ll = purrr::pmap(.l = list(d = .$d,
                                                   parameters = .$parameters),
                                         .f = function(d, parameters) {
                                           itchmodel::db_bin_choice_prob(d = d,
                                                                         s = rep(1,length(d)),
                                                                         a = rep(as.double(parameters["a"]), length(d)),
                                                                         z = rep(0,length(d)))}
                                         )
                      # med_rt = purrr::pmap_dbl(.l = list(d = .$d,
                      #                                    parameters = .$parameters),
                      #                          .f = function(d, parameters) {
                      #                            rtdists::qdiffusion(v = d,
                      #                                                a = as.double(parameters["a"]),
                      #                                                t0 = (as.double(parameters["t0"])),
                      #                                                z = as.double(parameters["a"]) * 0.5,
                      #                                                response = "upper", # For typical values, predictions for median RTs were found not to differ between "lower" and "upper" responses, hence only "upper"
                      #                                                p = rep(0.5, length(d)),
                      #                                                scale_p = TRUE
                      #                                                )
                      #                            }
                      #                          )
                      )
    }
  }

  if (return_var == "obs") {
    return(list(obs_nested =
                  obs_nested))
  } else if (return_var == "prd") {
    return(list(prd_nested =
                  prd_nested %>%
                  dplyr::select(participant_id, frame, stimuli, p_ll)
    )
    )
  } else if (return_var == "all") {
    return(list(obs_nested =
                  obs_nested,
                prd_nested =
                  prd_nested %>%
                  dplyr::select(participant_id, frame, stimuli, p_ll),
                all_nested =
                  prd_nested
    )
    )
  }
}
