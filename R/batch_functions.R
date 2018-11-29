# ==============================================================================

#' Batch-render analysis notebooks for multiple participants
#'
#' A notebook will be run
#' @param notebook_file filename of the template notebook to be run
#' @param notebook_dir directory where the template notebook resides
#' @param reports_dir directory where reports are written
#' @param params_tibble tibble of parameter values with which to run the notebooks
#' @param force whether or note to rerun a notebook when it exists
#' @export
render_notebook <- function(notebook_file, notebook_dir = "analysis", reports_dir = "reports", params_tibble, force = FALSE) {

  # Check if notebook_dir and reports_dir exist, create them if they don't
  check_dir(all_dirs = c(notebook_dir, reports_dir))

  # Parse notebook file
  notebook_path <-
    file.path(notebook_dir, notebook_file)
  notebook_fileparts <-
    stringr::str_split(notebook_file, pattern = "\\.", simplify = TRUE)
  notebook_filename <-
    notebook_fileparts[1:length(notebook_fileparts) - 1]
  notebook_ext <-
    notebook_fileparts[-1]

  # Assertions
  assertthat::assert_that(file.exists(notebook_path),
                          msg = stringr::str_c("Notebook '",
                                               notebook_path,
                                               "' does not exist.")
                          )

  yaml_params <-
    rmarkdown::yaml_front_matter(input = notebook_path)$params



  for (i_row in 1:nrow(params_tibble)) {

    # Parameters to override in the YAML frontmatter of notebook
    run_params <-
      params_tibble %>%
      dplyr::slice(i_row) %>%
      as.list()


    if ("participant_id" %in% names(run_params)) {
      pid_str <- sprintf("pid-%.3d", run_params$participant_id)
    } else{
      pid_str <- sprintf("pid-%.3d", yaml_params$participant_id)
    }

    if ("model_name" %in% names(run_params)) {
      model_str <- sprintf("model-%s", stringr::str_replace(run_params$model_name, "_", ""))
    } else{
      model_str <- sprintf("model-%s", stringr::str_replace(yaml_params$model_name, "_", ""))
    }

    if ("parameterization" %in% names(run_params)) {
      pmz_str <- sprintf("pmz-%s", run_params$parameterization)
    } else{
      pmz_str <- sprintf("pmz-%s", yaml_params$parameterization)
    }

    if ("bound_setting" %in% names(run_params)) {
      bound_str <- sprintf("bounds-%s", run_params$bound_setting)
    } else{
      bound_str <- sprintf("bounds-%s", yaml_params$bound_setting)
    }

    if ("algorithm" %in% names(run_params)) {
      algorithm <- sprintf("algorithm-%s", run_params$algorithm)
    } else{
      algorithm <- sprintf("algorithm-%s", yaml_params$algorithm)
    }

    if ("maxiter" %in% names(run_params)) {
      max_iter <- sprintf("maxiter-%d", run_params$max_iter)
    } else{
      max_iter <- sprintf("maxiter-%d", yaml_params$max_iter)
    }

    if ("rel_tol" %in% names(run_params)) {
      rel_tol <- sprintf("rel_tol-%.0e", run_params$rel_tol)
    } else{
      rel_tol <- sprintf("rel_tol-%.0e", yaml_params$rel_tol)
    }

    if ("n_pop_per_free_param" %in% names(run_params)) {
      n_pop_per_free_param <- sprintf("n_pop_per_free_param-%d", run_params$n_pop_per_free_param)
    } else{
      n_pop_per_free_param <- sprintf("n_pop_per_free_param-%d", yaml_params$n_pop_per_free_param)
    }

    if ("optimize" %in% names(run_params)) {
      optim_str <- sprintf("optimize-%d", as.integer(run_params$optimize))
    } else {
      optim_str <- sprintf("optimize-%d", as.integer(yaml_params$optimize))
    }

    if ("visualize" %in% names(run_params)) {
      vis_str <- sprintf("visualize-%d", as.integer(run_params$visualize))
    } else {
      vis_str <- sprintf("visualize-%d", as.integer(yaml_params$visualize))
    }

    suffix_str <- c(pid_str, model_str, pmz_str, bound_str, algorithm,
                    max_iter, optim_str, vis_str)

    nonempty_str <- stringr::str_length(suffix_str) > 0
    suffix_str <- stringr::str_flatten(suffix_str[nonempty_str], collapse = "_")
    suffix_str <- stringr::str_c(suffix_str, ".html")

    report_file <-
      stringr::str_c(notebook_filename,
                     suffix_str,
                     sep = "_")
    report_path <- file.path(reports_dir, report_file)

    # Render notebook only if report does not exist or if forced
    if (!file.exists(report_path) | force) {
      tryCatch(rmarkdown::render(input = notebook_path,
                                 output_dir = reports_dir,
                                 output_file = report_file,
                                 knit_root_dir =
                                   rprojroot::find_root(rprojroot::has_file("DESCRIPTION")),
                                 params = run_params
                                 ),
               error = function(e) print(sprintf("Failed to render notebook for participant %d.",
                                                 run_params$participant_id))
               )

    }
  }

}

