#' @title Delete old R temporary directories
#'
#' @description Delete all R temporary directories except for current
#' directory.
#'
#' @export
delete_old_tempdir <- function() {
  to_remove <- setdiff(dir(dirname(tempdir()), recursive = FALSE, full.names = FALSE,
                           pattern = "^Rtmp"),
                       basename(tempdir()))
  purrr::walk(
    to_remove,
    function(x) {
      unlink(
        file.path(dirname(tempdir()), x),
        recursive = TRUE
      )
    }
  )

  invisible(TRUE)
}

