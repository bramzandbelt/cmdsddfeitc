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
  subtitle_font_size <- 15 # 18
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
