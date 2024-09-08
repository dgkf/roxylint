#' If-not-null-else
#'
#' @name if-not-null-else
#' @keywords internal
`%||%` <- function(lhs, rhs) if (is.null(lhs)) rhs else lhs


#' Get that tag!
#'
#' Tools for inspecting [roxygen2::roxy_tag()]s. This can be helpful for
#' exploring the intermediate tag objects. For example, you can use this
#' function to generate a tag and explore the named values in `$val`.
#'
#' @param str A 'roxygen2' tag string.
#'
#' @return A [roxygen2::roxy_tag()].
#'
#' @examples
#' demo_tag("@param var abc")
#'
#' @export
demo_tag <- function(str) {
  str <- strsplit(str, "\n")[[1]]
  code <- paste0(paste0("#' ", str, collapse = "\n"), "\nNULL")
  res <- roxygen2::parse_text(code)
  roxygen2::roxy_tag_parse(res[[1]]$tags[[1]])
}


#' Capture `# nolint` in strings
#'
#' @param x `character` vector, with strings possibly containing an `r # nolint` 
#'   declaration.
#' @return A `list` of length equal to that of parameter `x`, giving itemized
#'   lint names in cases of matches, `TRUE` if a blanket `# nolint` declaration
#'   is provided and `FALSE` if there is no matched `# nolint` declaration at
#'   all.
#'
capture_nolints <- local({
  re_nolint <- paste0(
    "(`+)",    "\\s*", # opening code block (+ possible whitespace)
    "r",       "\\s*", # r language indicator (+ possible whitespace)
    "#",       "\\s*", # containing only a comment (+ possible whitespace)
    "nolint",  "\\s*", # that starts with nolint (+ possible whitespace)
    ":?",              # colon optional (+ possible whitespace)
    "(?<nolints>.*)",  # capture all text until closing backticks
    "\\1"
  )

  function(x) {
    m <- regexpr(re_nolint, x, perl = TRUE)
    nolint_start <- attr(m, "capture.start")[, "nolints"]
    nolint_nchar <- attr(m, "capture.length")[, "nolints"]
    nolints <- substring(x, nolint_start, nolint_start + nolint_nchar - 1L)
    nolints <- gsub("[:.,]", " ", nolints)
    out <- lapply(strsplit(nolints, " "), Filter, f = nchar)
    out[nolint_start < 0] <- FALSE
    out[vapply(out, length, integer(1L)) == 0] <- TRUE
    out
  }
})



#' Scrubs a string of texinfo and markdown markup
#'
#' Superceded by `tag_text` but kept around in case the alternatives ever
#' break down for some edge case.
#'
#' @keywords internal
scrub_markup <- local({
  # all patterns expected to place entire match with **second** capture group
  #
  # extra capture groups are included solely for the sake of making calculations
  # easier.
  #
  markups <- paste(
    sep = "|",
    # texinfo markup
    "()()\\\\(?:code|preformatted){[^{}]*(?R)[^{}]*}",
    # markdown bold/italics
    "(\\*+)(.*)\\g{-2}",
    # markdown code blocks
    "(`+)(.*)\\g{-2}",
    # markdown-style link syntax
    "()\\[.*\\]\\((.*)\\)",
    # roxygen2-style link syntax
    "()\\[(.*)\\]"
  )

  function(x) {
    Map(
      function(x, m) {
        if (m == -1) return(x)
        l <- attr(m, "match.length")

        # match
        m_start <- m[1, ]
        m_nchar <- l[1, ]

        # replacement
        r_start <- apply(m[-1, ][c(FALSE, TRUE), ], 2, sum)
        r_nchar <- apply(l[-1, ][c(FALSE, TRUE), ], 2, sum)

        # use a for loop here because this seems to not be vectorized even
        # though it seems to indicate it is in the docs?
        for (i in rev(seq_along(r_start))) {
          substring(x, m_start[[i]], m_start[[i]] + m_nchar[[i]] - 1L) <-
            substring(x, r_start[[i]], r_start[[i]] + r_nchar[[i]] - 1L)
        }

        x
      },
      x,
      gregexec(markups, x, perl = TRUE),
      USE.NAMES = FALSE
    )
  }
})

tag_rd <- function(tag) {
  rd <- roxygen2::roxy_tag_rd(tag)
  if (is.null(rd)) return(tag$raw)
  suppressWarnings(tools::parse_Rd(
    textConnection(format(rd)),
    warningCalls = FALSE,
    permissive = TRUE
  ))
}

#' Parse a [`roxygen2`] tag into its visible text
#'
#' Optionally prunes specific `Rd` tags whose rendered content should not
#' affect linting.
#'
#' @param tag A [`roxygen2`] tag. Expected to have an implementation of
#'   [`roxygen2::roxy_tag_rd`] and its result should implement [`format`].
#' @return A `character` string of visible text.
#'
#' @keywords internal
rd_text <- function(
  rd,
  prune = c(
    "\\code",
    "\\preformatted",
    "\\file",
    "\\email",
    "\\url",
    "\\cite",
    "\\acronym"
  )
) {
  rd <- prune_rd(rd, prune)
  paste(unlist(rd), collapse = "")
}

#' Prune specific tags from ending up in linted text
#'
#' @keywords internal
prune_rd <- function(rd, prune) {
  if (is.list(rd)) {
    tag_names <- vapply(rd, attr, character(1L), "Rd_tag")
    Filter(
      Negate(is.null),
      lapply(rd[!tag_names %in% prune], prune_rd, prune = prune)
    )
  } else if (isTRUE(attr(rd, "Rd_tag") %in% prune)) {
    NULL
  } else {
    rd
  }
}

#' Borrowed from [`Hmisc`]
re_escape <- function(x) {
  gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", x)
}
