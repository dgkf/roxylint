#' Apply 'roxygen2' Linters `r # nolint: sentence_case.`
#'
#' Provided a list of linters for a given tag, iterate over linters to raise
#' alerts during the documentation process.
#'
#' @param linters A linters specification, either a `function` or
#'   `character` regular expression that should be matched, or a `list`
#'   of either. See details for more information.
#' @param tag A [roxygen2::roxy_tag()].
#' @param message An optional message to use for an alert.
#' @param ... Additional arguments unused.
#'
#' @return `TRUE`, invisibly. However, this function is primarily used for its
#'   side-effect of raising alerts during documentation.
#'
#' @export
check_linter <- function(linters, tag, ...) {
  # content was generated automatically with no originating text
  if (is.null(tag$raw)) return(invisible(TRUE))
  UseMethod("check_linter")
}


#' @describeIn check_linter
#' By default, no linting is performed
#'
#' @export
check_linter.default <- function(linters, tag, ...) {
  invisible(TRUE)
}


#' @describeIn check_linter
#' A `list` of `function`s or `character` regular expressions.
#'
#' If a `character` value is named, the name is used as the message for a alert
#' that is raised when the expression does not match.
#'
#' @export
check_linter.list <- function(linters, tag, ...) {
  for (i in seq_along(linters)) {
    linter <- linters[[i]]
    message <- names(linters[i])
    message <- if (!is.null(message) && nchar(message) > 0) message else NULL
    check_linter(linter, tag, message = message, ...)
  }
  invisible(TRUE)
}


#' @describeIn check_linter
#' A `function` to evaluate for the given tag
#'
#' `function`'s are evaluated with the following arguments:
#'
#'   1. The [roxygen2::roxy_tag()]
#'   2. The contents of the tag's `$val`, as named arguments
#'
#' Because the number of arguments might not be readily apparent, any function
#' should accept a trailing `...` argument.
#'
#' Provided `function`s may print lint output, or signal lint output with
#' messages or warnings. A [cli::cli_alert()] will reflect the severity of the
#' function used to emit the output.
#'
#' @export
check_linter.function <- function(linters, tag, ...) {
  args <- list(tag, ...)
  args <- append(args, tag$val)  # append parsed terms from roxy tag
  into_roxy_alert(tag, do.call(linters, args))
  invisible(TRUE)
}


#' @describeIn check_linter
#' A `character` regular expressions.
#'
#' If a `character` value is found, its value is assumed to be a regular
#' expression which must match a given tag's `raw` content (the text as it
#' appears in the `roxygen2` header).
#'
#' @export
check_linter.character <- function(linters, tag, message = NULL, ...) {
  into_roxy_alert(tag, {
    if (!grepl(linters, tag$raw, perl = TRUE)) {
      message <- message %||% paste0("raw value does not match '", linters, "'")
      message(message)
    }
  })
  invisible(TRUE)
}
