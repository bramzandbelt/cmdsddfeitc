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
