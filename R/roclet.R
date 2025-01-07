#' Roclet for 'roxygen2' style linting
#'
#' Roclets used to embed linters during documentation. To use, add the roclet in
#' your `DESCRIPTION` file.
#'
#' ```
#' Config/Needs/documentation: roxylint
#' Roxygen:
#'   list(
#'     markdown = TRUE,
#'     roclets = c("namespace", "rd", "roxylint::roxylint")
#'   )
#' ```
#'
#' @return A `roxylint` [roxygen2::roclet()].
#'
#' @export
roxylint <- function() {
  roxygen2::roclet("roxylint")
}


#' @exportS3Method roxygen2::roclet_process roclet_roxylint
#' @noRd
roclet_process.roclet_roxylint <- function(x, blocks, env, base_path) {  # nolint
  config <- config_load(path = base_path)

  for (block in blocks) {
    # discover @concepts defined in the block & pass to config
    concepts <- roxygen2::block_get_tags(block, "concept")
    concepts <- unlist(lapply(concepts, `[[`, "val"))
    config$concepts <- concepts

    for (x in block$tags) {
      linters <- config$linters[[x$tag]]
      check_linter(linters, x, config = config)
    }
  }

  invisible(NULL)
}


#' @exportS3Method roxygen2::roclet_output roclet_roxylint
#' @noRd
roclet_output.roclet_roxylint <- function(...) {  # nolint
  invisible(NULL)
}
