# ==============================================================================

#' Check whether directories exists, and create recursively if they don't
#'
#' @param all_dirs Character vector of directory paths to check
#' @export
check_dir <- function(all_dirs){

  for (this_dir in all_dirs) {
    if (!dir.exists(this_dir)) {
      dir.create(this_dir, recursive = TRUE)
    }
  }

}
