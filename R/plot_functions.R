require(Hmisc)

## get_aes_values ##############################################################
#' Get project-specific aesthetics values
#'
#' For instance, parameterization-dependent color and fills
#'
#' @export
get_aes_values <- function(aesthetic, parameterization) {

  if (aesthetic %in% c("color", "fill")) {
    if (stringr::str_detect(parameterization, "defer_speedup")) {
      # Delay: blue
      # Deferral: orange
      # Speedup: green
      return(c("#56B4E9", "#E69F00", "#009E73"))

    } else if (stringr::str_detect(parameterization, "date_delay")) {
      # Delay: blue
      # Date: green
      return(c("#56B4E9", "#009E73"))

    } else if (stringr::str_detect(parameterization, "one_condition")) {
      return("#56B4E9")
    }
  } else if (aesthetic == "shape") {
    if (stringr::str_detect(parameterization, "defer_speedup")) {
      # Delay: circle
      # Deferral: plus
      # Speedup: cross
      return(c(1,3,4))

    } else if (stringr::str_detect(parameterization, "date_delay")) {
      # Delay: circle
      # Date: plus
      return(c(1,3))

    } else if (stringr::str_detect(parameterization, "one_condition")) {
      # Alwaus circle
      return(c(1))
    }
  }

}

## theme_cmfsddfeitc ###########################################################
#' Set project-specific ggplot2 theme
#'
#'
#'
#' @export
theme_cmfsddfeitc <- function() {
  title_font_size <- 18 # 24
  subtitle_font_size <- 14 # 18
  label_font_size <- 12 # 15

  ggthemes::theme_few() +
    ggthemes::theme_few() +
    ggplot2::theme(

      aspect.ratio = 1/1.61,

      plot.title = ggplot2::element_text(face = "bold",
                                         size = title_font_size,
                                         hjust = 0.5),
      axis.line = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(size = label_font_size),
      axis.text.x = ggplot2::element_text(),
      axis.ticks = ggplot2::element_blank(),
      axis.title = ggplot2::element_text(size = subtitle_font_size),

      legend.background = ggplot2::element_rect(),
      legend.text = ggplot2::element_text(size = label_font_size),
      legend.title = ggplot2::element_text(size = subtitle_font_size),

      panel.background = ggplot2::element_rect(fill = "gray95",
                                               color = NA),
      panel.border = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.spacing = ggplot2::unit(0.02,"in"),

      strip.background = ggplot2::element_rect(),
      strip.text = ggplot2::element_text(size = label_font_size),
      # strip.background = ggplot2::element_blank(),
      # strip.text = ggplot2::element_blank()
    )
  }



## plot_ips_calibration ########################################################
#' Plot indifference points from the calibration task as a function of delay
#'
#' Individual indifference points, as well as group median and IQR. This plot
#' function is used in notebook 05_eda_grp.Rmd.
#'
#' @params tibb, tibble containing cols subject_ix <int>, t_l <int>, ip <dbl>
#' @export
plot_ips_calibration <- function(tibb) {

  # General setup --------------------------------------------------------------
  tibb %>% {

    ggplot2::ggplot(data = .,
                    mapping = ggplot2::aes(x = t_l,
                                           y = ip)
                    ) +

      # Geoms --------------------------------------------------------------------

      ggplot2::geom_line(mapping = ggplot2::aes(group = subject_ix),
                         size = 0.5,
                         alpha = 0.2) +


      # Summary statistics -------------------------------------------------------
      ggplot2::stat_summary(geom = "errorbar",
                            fun.data = median_hilow,
                            fun.args = list(conf.int=.5),
                            color = "black",
                            size = 0.5) +

      ggplot2::stat_summary(mapping = ggplot2::aes(shape = "median"),
                            geom = "point",
                            size = 2,
                            shape = 21,
                            stroke = 0.5,
                            fun.data = median_hilow,
                            fun.args = list(conf.int=.5),
                            color = "black") +

      # Scales -------------------------------------------------------------------
      ggplot2::scale_x_continuous(name = "Delay (days)",
                                  limits = c(0,128),
                                  breaks = c(0,2,4,8,16,32,64,128),
                                  labels = c("","2","","","","32","64","128")) +
      ggplot2::scale_y_continuous(name = "Indifference point (€)",
                                  limits = c(0,43.52)) +

      # Annotation -------------------------------------------------------------
      ggplot2::annotate(geom = "text",
                        label = paste("N =", nrow(tibb %>% dplyr::filter(t_l == 2))),
                        x = 128,
                        y = 2,
                        hjust = 1,
                        vjust = 1
      ) +

      # Themes -------------------------------------------------------------------
      cmdsddfeitc::theme_cmfsddfeitc() +
        ggplot2::theme(panel.background = ggplot2::element_blank())

  }



}

## plot_ll_choice_pctgs ########################################################
#' Plot larger-later choice percentage across frames
#'
#' This plot function is used in notebook 05_eda_grp.Rmd.
#'
#' @params tibb, tibble containing cols subject_ix <int>, t_l <int>, ip <dbl>
#' @params task, chr array indicating task ("defer_speedup", "date_delay")
#' @export
plot_ll_choice_pctgs <- function(tibb, task) {

  # Task-dependent settings
  if (task == "defer_speedup") {
    x <- "defer"
    y <- "speedup"
    xlab <- "Pr(LL|defer)"
    ylab <- "Pr(LL|speedup)"
  } else if (task == "date_delay") {
    x <- "delay"
    y <- "date"
    xlab <- "Pr(LL|delay)"
    ylab <- "Pr(LL|date)"
  }

  sum_stats_fun <- function(tibb, errorbartype) {

    if (errorbartype == "horizontal") {
      tibb %>%
        dplyr::summarize(!!y :=  median(!!sym(y)),
                         !!x := median(!!sym(x)),
                         xmin = quantile(!!sym(x), probs = .25),
                         xmax = quantile(!!sym(x), probs = .75)
        )
    } else if (errorbartype == "vertical") {
      tibb %>%
        dplyr::summarize(!!x := median(!!sym(x)),
                         !!y := median(!!sym(y)),
                         ymin = quantile(!!sym(y), probs = .25),
                         ymax = quantile(!!sym(y), probs = .75)
                         )
    }
  }

  sum_stats_x <- sum_stats_fun(tibb = tibb, errorbartype = "horizontal")
  sum_stats_y <- sum_stats_fun(tibb = tibb, errorbartype = "vertical")



    # Input ----------------------------------------------------------------------
  tibb %>% {


    ggplot2::ggplot(data = .,
                    mapping = ggplot2::aes(x = !!sym(x),
                                           y = !!sym(y))
    ) +

      # Geoms ------------------------------------------------------------------
      ggplot2::geom_point(shape = 21,
                          size = 2,
                          stroke = 0.5,
                          alpha = 0.2) +
      ggplot2::geom_abline(slope = 1,
                           intercept = 0,
                           linetype = "dashed") +

      ggplot2::geom_errorbar(mapping = ggplot2::aes(ymin=quantile(!!sym(y), probs = .25),
                                                    ymax=quantile(!!sym(y), probs = .75),
                                                    x = median(!!sym(x)),
                                                    group = 1),
                             size = 0.5
                             ) +
      ggplot2::geom_errorbarh(mapping = ggplot2::aes(xmin=quantile(!!sym(x), probs = .25),
                                                     xmax=quantile(!!sym(x), probs = .75),
                                                     y = median(!!sym(y)),
                                                     group = 1),
                              size = 0.5
                              ) +

      ggplot2::geom_point(mapping = ggplot2::aes(x = median(!!sym(x)),
                                                 y = median(!!sym(y)),
                                                 group = 1),
                          size = 2,
                          shape = 21,
                          stroke = 0.5
                          ) +

      # Scales -----------------------------------------------------------------
      ggplot2::scale_x_continuous(limits = c(0,1),
                                  breaks = seq(0,1, 0.5),
                                  name = xlab) +
      ggplot2::scale_y_continuous(limits = c(0,1),
                                  breaks = seq(0,1, 0.5),
                                  name = ylab) +

      # Themes -----------------------------------------------------------------
    cmdsddfeitc::theme_cmfsddfeitc() +
      ggplot2::theme(aspect.ratio = 1,
                     panel.background = ggplot2::element_blank())
  }
}


## plot_rt_percentiles #########################################################
#' Plot response time percentiles across frames
#'
#' This plot function is used in notebook 05_eda_grp.Rmd.
#'
#' @params tibb, tibble containing cols participant_id, percentile, and frames
#' @params task, chr array indicating task ("defer_speedup", "date_delay")
#' @export
#'
plot_rt_percentiles <- function(tibb, task) {

  # Task-specific settings
  if (task == "defer_speedup") {
    x <- "defer"
    y <- "speedup"
  } else if (task == "date_delay") {
    x <- "delay"
    y <- "date"
  }

  xlab <- sprintf("RT %s (s)", x)
  ylab <- sprintf("RT %s (s)", y)
  x_nm <- dplyr::quo_name(x)
  y_nm <- dplyr::quo_name(y)

  # Compute mean percentile across participants
  rt_summary_stats <-
    tibb %>%
    dplyr::ungroup() %>%
    dplyr::group_by(percentile) %>%
    dplyr::summarize(!!x_nm := mean(!!sym(x)),
                     !!y_nm := mean(!!sym(y))
    )

  ggplot2::ggplot(data = tibb,
                  mapping = ggplot2::aes(x = !!sym(x),
                                         y = !!sym(y))) +
    ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
    ggplot2::geom_line(alpha = 0.2,
                       mapping = ggplot2::aes(group = participant_id),
                       size = 0.5) +
    ggplot2::geom_point(mapping = ggplot2::aes(color = percentile),
                        size = 2,
                        shape = 21,
                        stroke = 0.5,
                        alpha = 0.2) +
    ggplot2::geom_line(data = rt_summary_stats,
                       size = 0.5) +
    ggplot2::geom_point(data = rt_summary_stats,
                        mapping = ggplot2::aes(color = percentile,
                                               fill = percentile),
                        size = 2,
                        shape = 21,
                        stroke = 0.5) +
    ggrepel::geom_text_repel(data = rt_summary_stats,
                             mapping = ggplot2::aes(label = percentile,
                                                    color = percentile),
                             segment.alpha = 0,
                             direction = "both",
                             nudge_x = 3) +
    ggplot2::scale_x_continuous(name = xlab,
                                limits = c(1.5, 10)) +
    ggplot2::scale_y_continuous(name = ylab,
                                limits = c(1.5, 10)) +
    ggplot2::coord_equal() +
    cmdsddfeitc::theme_cmfsddfeitc() +
    ggplot2::theme(aspect.ratio = 1,
                   legend.position = "none",
                   panel.background = ggplot2::element_rect(fill = NA,
                                                            color = NA)
    )
}

## plot_model_comparison_aggregate_bic #########################################
#' Plot results of model comparison based on aggregate BIC score
#'
#'
#' @params tibb, tibble containing the data to be plotted
#' @params fill_values, array containing color names or HEX codes to use for different levels of k
#' @export
plot_model_comparison_aggregate_bic <- function(tibb, fill_values) {

  # Make plot of model comparison based on aggregate BIC
  ggplot2::ggplot(data = tibb,
                  mapping = ggplot2::aes(x = parameterization,
                                         y = dBIC,
                                         fill = k)) +
    # Facets -------------------------------------------------------------------
    ggplot2::facet_grid(model ~ ., space = "free_y", scales = "free_y", switch = "y") +

    # Geoms --------------------------------------------------------------------
    ggplot2::geom_bar(stat = "identity") +

    # Scales -------------------------------------------------------------------
    ggplot2::scale_x_discrete(name = "Parameterization") +
    ggplot2::scale_y_continuous(name = "\u0394BIC",
                                limits = c(0, 1750),
                                breaks = seq(0,1500,500)) +
    ggplot2::scale_fill_manual("k",
                               values = fill_values) +
    ggplot2::coord_flip() +

    # Themes -------------------------------------------------------------------
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.border = ggplot2::element_rect(color = "black", fill = NA),
                   panel.spacing.y = ggplot2::unit(0,"line"),
                   panel.grid.major.y = ggplot2::element_blank(),
                   strip.placement = "outside",
                   strip.background.x = ggplot2::element_blank())
}

## plot_model_comparison_bic_count #############################################
#' Plot results of model comparison based on BIC counts
#'
#'
#' @params tibb, tibble containing the data to be plotted
#' @params fill_values, array containing color names or HEX codes to use for different levels of k
#' @export
plot_model_comparison_bic_count <- function(tibb, fill_values) {

  ggplot2::ggplot(data = tibb,
                  mapping = ggplot2::aes(x = parameterization,
                                         y = count,
                                         fill = k)) +
    ggplot2::facet_grid(model ~ ., space = "free_y", scales = "free_y", switch = "y") +

    # Geoms
    ggplot2::geom_bar(stat = "identity") +

    # Scales
    ggplot2::scale_x_discrete(name = NULL,
                              breaks = NULL) +
    ggplot2::scale_y_continuous(name = "Count",
                                limits = c(0, 10),
                                breaks = seq(0,10,2)) +
    ggplot2::scale_fill_manual("k",
                               values = fill_values) +
    ggplot2::coord_flip() +

    # Themes
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.border = ggplot2::element_rect(color = "black", fill = NA),
                   panel.spacing.y = ggplot2::unit(0,"line"),
                   panel.grid.major.y = ggplot2::element_blank(),
                   strip.placement = "outside",
                   strip.text = ggplot2::element_blank(),
                   strip.background.x = ggplot2::element_blank())

}


## plot_obs_prd_performance ####################################################
#' Plot performance observed against performance predicted by the model
#'
#'
#' @params tibb, tibble containing the data to be plotted
#' @params plotvar, variable to plot (p_ll or med_rt)
#' @export

# Plot function
plot_obs_prd_performance <- function(tibb, plotvar = "p_ll", key_frames_only = FALSE) {

  if (plotvar == "p_ll") {
    xvar <- "p_ll_obs"
    yvar <- "p_ll_prd"
    xlab <- "P(LL) observed"
    ylab <- "P(LL) predicted"
    xlim <- ylim <- c(0,1)
    xbreaks <- ybreaks <- c(0,1)
  } else if (plotvar == "med_rt") {
    xvar = "med_rt_obs"
    yvar = "med_rt_prd"
    xlab <- "Mdn RT (s) observed"
    ylab <- "Mdn RT (s) predicted"
    xlim <- ylim <- c(0,6)
    xbreaks <- ybreaks <- c(1, 3, 5)
  }

  if (key_frames_only) {
    tibb <-
      tibb %>%
      dplyr::filter(frame %in% c("defer", "speedup", "date", "delay"))
  }

  # Core of the display --------------------------------------------------------
  plt <-
    ggplot2::ggplot(data = tibb,
                    mapping = ggplot2::aes(x = !!dplyr::sym(xvar),
                                           y = !!dplyr::sym(yvar)))

  # Facets ---------------------------------------------------------------------
  if ("t_l" %in% colnames(tibb)) {
    plt <-
      plt +
      ggplot2::facet_grid(t_l ~ frame)
  } else {
    plt <-
      plt +
      ggplot2::facet_grid(. ~ frame)
  }

  # Geoms ----------------------------------------------------------------------
  if ("t_l" %in% colnames(tibb)) {
    pt_size <- 2
  } else {
    pt_size <- 3
  }

  plt <-
    plt +
    ggplot2::geom_point(shape = 1,
                        size = pt_size) +
    ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed") +

    # Scales ---------------------------------------------------------------------
  ggplot2::scale_x_continuous(name = xlab,
                              breaks = xbreaks,
                              limits = xlim) +
    ggplot2::scale_y_continuous(name = ylab,
                                breaks = ybreaks,
                                limits = ylim) +
    ggplot2::coord_equal() +

    # Themes ---------------------------------------------------------------------
  cmdsddfeitc::theme_cmfsddfeitc() +
    ggplot2::theme(aspect.ratio = 1,
                   panel.background =
                     ggplot2::element_rect(fill = NA,
                                           color = NA)
                   )

  # Return variable
  plt
}


## plot_par_values_distribution ####################################################
#' Plot distribution of best-fitting parameter values
#'
#'
#' @params tibb, tibble containing the data to be plotted in long format
#' @export

plot_par_values_distribution <- function(tibb) {
  ggplot2::ggplot(data = tibb,
                  mapping = ggplot2::aes(x = key,
                                         y = value)) +

    # Geoms --------------------------------------------------------------------
    ggplot2::geom_violin(scale = "width", alpha = 0.5) +
    ggbeeswarm::geom_quasirandom(alpha = 0.5) +
    ggplot2::stat_summary(fun.y = "median",
                          geom = "point",
                          color = "red",
                          fill = "red",
                          shape = 21,
                          na.rm = TRUE) +

    # Scales -------------------------------------------------------------------
    ggplot2::scale_x_discrete(name = "Free parameter name") +
    ggplot2::scale_y_continuous(name = "Parameter value") +

    ggplot2::coord_flip() +

    # Themes -------------------------------------------------------------------
    cmdsddfeitc::theme_cmfsddfeitc() +
    ggplot2::theme(panel.background = ggplot2::element_blank(),
                   panel.grid = ggplot2::element_blank())
}


## plot_par_values_pairs #######################################################
#' Plot distribution of best-fitting parameter values
#'
#'
#' @params tibb, tibble containing the data to be plotted in wide format
#' @export
plot_par_values_pairs <- function(tibb) {

  # General setup --------------------------------------------------------------
  GGally::ggpairs(tibb,
                  progress = FALSE,
                  columns = dplyr::select(tibb, -participant_id) %>% colnames(),
                  # Plots on the diagonal
                  # diag = list(continuous = "barDiag"),
                  diag = list(continuous = GGally::wrap('barDiag', bins = 10)),
                  # Plots below the diagonal
                  lower = list(continuous = "smooth"),
                  # Plots above the diagonal
                  upper = list(continuous = GGally::wrap('cor', method = "spearman"))) +

    # Themes -------------------------------------------------------------------
    cmdsddfeitc::theme_cmfsddfeitc() +
    ggplot2::theme(panel.background = ggplot2::element_blank(),
                   panel.grid = ggplot2::element_blank(),
                   axis.text = ggplot2::element_text(size = 8),
                   strip.text = ggplot2::element_text(size = 12))

}
