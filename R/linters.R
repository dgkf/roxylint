#' Assorted linters
#'
#' Preconfigured linters, either as a collective list of linters or
#' individually. "tidy" linters implement guidelines from the tidyverse style
#' guide.
#'
#' Refer to the individual [roxygen2::roxy_tag()] for the respective tag for
#' argument details.
#'
#' @param x A [roxygen2::roxy_tag()] that is the subject of linting.
#' @param name,description Used for [roxygen2::roxy_tag()]-specific linters.
#' @param ... Additional arguments unused.
#'
#' @name linters
NULL

#' Define a lint generic
#'
#' Attach optional attributes to a linting function. Most commonly used for
#' providing attributes:
#'
#' * `name`: A string used for disabling via `# nolint: <name>.`
#'
#' @keywords internal
lint <- function(f, ...) {
  structure(f, ..., class = c("lint", class(f)))
}

#' Find the last called lint function
#'
#' @return `function` the most recent function on the stack of type `lint`.
#'
#' @keywords internal
find_lint_generic <- function(skip = 1) {
  if (skip > sys.nframe()) return()
  for (i in seq(from = skip, to = sys.nframe())) {
    if (inherits(f <- sys.function(which = -i), "lint")) {
      return(f)
    }
  }
}

#' Check whether the current lint is disabled
#'
#' Scans raw text of tag for `` `r # nolint` or `r # nolint: <type>.` ``
#'
#' @param lint_name `character` name of the lint. Used if only specific lints
#'   are disabled for the provided tag.
#' @param fn `function` to use to test for lint disabling. The
#'   `"name"` attribute of this function will be used to infer the lint
#'   name. Defaults to the most recent `lint` function evaluated on the call
#'   stack.
#'
#' @return `logical` flag indicating whether this lint has been disabled for
#'   the given tag.
#'
#' @keywords internal
is_lint_disabled <- function(
  x,
  lint_name = attr(fn, "name"),
  fn = find_lint_generic()
) {
  if (inherits(x, "roxy_tag")) {
    x <- x$raw
  }

  disabled_lints <- capture_nolints(x)[[1]]
  if (isFALSE(disabled_lints)) {
    FALSE
  } else if (isTRUE(disabled_lints)) {
    # blanket #nolint declaration
    TRUE
  } else if (lint_name %in% disabled_lints) {
    # lint has been explicitly disabled
    TRUE
  } else {
    FALSE
  }
}

#' @describeIn linters
#' Lowercase start linting. (uses `$raw` for [roxygen2::roxy_tag()]s)
#'
#' @export
lint_starts_lowercase <- lint(
  name = "starts_lowercase",
  function(x, ...) {
    UseMethod("lint_sentence_case")
  }
)

#' @export
lint_starts_lowercase.default <- function(x, ...) {
}

#' @export
lint_starts_lowercase.roxy_tag <- function(x, ...) {
  if (is_lint_disabled(x)) return()

  # ignore if first element is not relevant to linter
  rdf <- rd_df(tag_rd(x))
  first_non_ws <- Position(identity, !rdf$is_whitespace)
  if (is.na(first_non_ws) || !isTRUE(rdf[first_non_ws, "lintable"])) {
    return()
  }

  lint_starts_lowercase(lintable_text(rdf), ...)
}

#' @export
lint_starts_lowercase.character <- function(x, ...) {
  re <- "^[^[:upper:]]"
  if (!grepl(re, trimws()))
    message("should not start with an uppercase letter")
}


#' @describeIn linters
#' Ends in a full stop. (uses `$raw` for [roxygen2::roxy_tag()]s)
#'
#' @export
lint_full_stop <- lint(
  name = "full_stop",
  function(x, ...) {
    UseMethod("lint_full_stop")
  }
)

#' @export
lint_full_stop.default <- function(x, ...) {
}

#' @export
lint_full_stop.roxy_tag <- function(x, ...) {
  if (is_lint_disabled(x$raw)) return()
  lint_full_stop(lintable_text(x), ...)
}

#' @export
lint_full_stop.character <- function(x, ...) {
  re <- "\\.$"
  if (!grepl(re, trimws(x)))
    message("should terminate with a full stop, `.`")
}


#' @describeIn linters
#' Does not end in a full stop. (uses `$raw` for [roxygen2::roxy_tag()]s)
#'
#' @export
lint_no_full_stop <- lint(
  name = "no_full_stop",
  function(x, ...) {
    UseMethod("lint_no_full_stop")
  }
)

#' @export
lint_no_full_stop.default <- function(x, ...) {
}

#' @export
lint_no_full_stop.roxy_tag <- function(x, ...) {
  if (is_lint_disabled(x$raw)) return()
  lint_no_full_stop(lintable_text(x), ...)
}

#' @export
lint_no_full_stop.character <- function(x, ...) {
  re <- "[^.]$"
  if (!grepl(re, trimws(x)))
    message("should not terminate with a full stop, `.`")
}


#' @describeIn linters
#' Sentence case linting (uses `$raw` for [roxygen2::roxy_tag()]s)
#'
#' @export
lint_sentence_case <- lint(
  name = "sentence_case",
  function(x, ...) {
    UseMethod("lint_sentence_case")
  }
)

#' @export
lint_sentence_case.default <- function(x, ...) {
}

#' @export
lint_sentence_case.roxy_tag <- function(x, ...) {
  if (is_lint_disabled(x$raw)) return()

  # ignore if first element is not relevant to linter
  rdf <- rd_df(tag_rd(x))
  first_non_ws <- Position(identity, !rdf$is_whitespace)
  if (is.na(first_non_ws) || !isTRUE(rdf[first_non_ws, "lintable"])) {
    return()
  }

  lint_sentence_case(lintable_text(x), ...)
}

#' @export
lint_sentence_case.character <- function(x, ...) {
  words <- strsplit(trimws(x), " ")[[1L]]
  words <- words[nchar(words) > 0]

  # find any first words in sentences (at start, or after full stop)
  has_stop <- grepl("\\.$", words)
  is_start <- rep_len(FALSE, length.out = length(words))
  is_start[[1]] <- TRUE
  is_start[-1] <- has_stop[-length(words)]

  first_cap <- all(grepl("^[^[:lower:]]", words[is_start]))
  rest_lower <- all(grepl("^[^[:upper:]]", words[!is_start]))

  if (!(first_cap && rest_lower))
    message("should be 'Sentence case'")
}


#' @describeIn linters
#' Title case linting
#'
#' @export
lint_title_case <- lint(
  name = "title_case",
  function(x, ...) {
    UseMethod("lint_title_case")
  }
)

#' @export
lint_title_case.default <- function(x, ...) {
}

#' @export
lint_title_case.roxy_tag <- function(x, ...) {
  if (is_lint_disabled(x$raw)) return()
  x <- rd_text(tag_rd(x))
  lint_title_case(x, ...)
}

#' @export
lint_title_case.character <- function(x, ...) {
  # AP style title case rules
  words <- strsplit(x, "\\s+")[[1L]]
  exceptions <- c(
    "a", "an", "the",  # articles
    "and", "but", "for",  # coordinating conjunctions
    "at", "by", "to", "of", "on", "off", "out"  # prepositions
  )

  is_exception <- tolower(words) %in% exceptions
  is_exception[[1]] <- FALSE
  is_exception[[length(words)]] <- FALSE

  if (any(grepl("^[[:lower:]]", words[!is_exception])))
    message("should be 'Title Case'")
}


#' @describeIn linters
#' Tidy 'Sentence case' titles
#'
#' @export
tidy_title <- function(x, ...) {
  lint_sentence_case(x)
  lint_no_full_stop(x)
}


#' @describeIn linters
#' Tidy 'Sentence case' `@param` definitions
#'
#' @export
tidy_param <- function(x, name, description, ...) {
  lint_sentence_case(description)
  lint_full_stop(description)
}


#' @describeIn linters
#' Tidy 'Sentence case' `@return` definitions
#'
#' @export
tidy_return <- function(x, ...) {
  lint_sentence_case(x)
  lint_full_stop(x)
}


#' @describeIn linters
#' Tidy 'Sentence case' `@seealso` definitions
#'
#' @export
tidy_seealso <- function(x, ...) {
  lint_sentence_case(x)
  lint_full_stop(x)
}


#' @describeIn linters
#' A list of all tidyverse style guide inspired linters
#'
#' @export
tidy <- list(
  title = tidy_title,
  param = tidy_param,
  return = tidy_return,
  seealso = tidy_seealso
)
