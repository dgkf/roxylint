lintable_text <- function(x) {
  if (!is.data.frame(x)) x <- rd_df(x)
  paste0(x$content[x$is_text & x$lintable], collapse = "")
}

rd_tag_df <- function(tag, is_text = TRUE, lintable = TRUE) {
  content <- paste0(format(tag), collapse = "")
  data.frame(
    tag = attr(tag, "Rd_tag"),
    content = content,
    is_text = is_text,
    is_whitespace = nchar(trimws(content)) == 0L,
    lintable = lintable
  )
}

rd_tag_text <- function(rd) rd_tag_df(rd)
rd_tag_non_text <- function(rd) rd_tag_df(rd, is_text = FALSE, lintable = FALSE)

rd_df <- function(rd) {
  if (!is.null(tag <- attr(rd, "Rd_tag"))) {
    class(rd) <- sub("\\\\", "\\", tag)
  }
  UseMethod("rd_df", rd)
}

#' @export
rd_df.roxy_tag <- function(rd) {
  rd_df(tag_rd(rd))
}

#' @export
rd_df.default <- function(rd) {
  if (is.list(rd)) {
    do.call(rbind, lapply(rd, function(rd) rd_df(rd)))
  } else {
    rd_tag_df(rd)
  }
}

#' @export
rd_df.href <- function(rd) {
  rd_df(rd[[1]])
}

#' @export
rd_df.dfn <- function(rd) {
  rd_tag_df(rd, is_text = TRUE, lintable = FALSE)
}

#' @export
rd_df.itemize <- function(rd) {
  is_item_sep <- vapply(
    rd,
    function(rdi) identical(attr(rdi, "Rd_tag"), "\\item"),
    logical(1L)
  )

  # split into distinct items, only separated by naked \\item tag
  items <- split(rd[!is_item_sep], cumsum(is_item_sep)[!is_item_sep])
  items <- lapply(items, `class<-`, "item")

  rd_df(items)
}

#' @export
rd_df.item <- function(rd) {
  if (length(rd) == 1) rd_df(rd[[1]]) else rd_df(rd[-1])
}

#' @export
rd_df.acronym <- function(rd) {
  rd_tag_df(rd, is_text = TRUE, lintable = FALSE)
}

#' @export
rd_df.preformatted <- rd_tag_non_text

#' @export
rd_df.code <- rd_tag_non_text

#' @export
rd_df.pkg <- rd_tag_non_text

#' @export
rd_df.kbd <- rd_tag_non_text

#' @export
rd_df.verb <- rd_tag_non_text

#' @export
rd_df.url <- rd_tag_non_text

#' @export
rd_df.file <- rd_tag_non_text

#' @export
rd_df.email <- rd_tag_non_text

#' @export
rd_df.env <- rd_tag_non_text

#' @export
rd_df.option <- rd_tag_non_text

#' @export
rd_df.cite <- rd_tag_non_text

rd_df_split_full_stops <- local({
  re_full_stop <- paste0(
    "(?!<\\bal|\\be\\.g)", # is not exception: "e.g.", "et al."
    "[.?!:]",              # a stop
    "(?:\\s+|$)"           # preceding space or end of string
  )

  function(rdf) {
    rdf <- rd_df_expand_content_re(
      rdf,
      re_full_stop,
      perl = TRUE,
      which = "right"
    )

    rdf$is_whitespace[rdf$is_text] <- grepl("^\\s*$", rdf$content[rdf$is_text])

    # group by sentence index
    sentence_group <- rev(cumsum(rev(rdf$is_match)))
    sentence_group <- max(sentence_group) - sentence_group + 1L

    rdf$is_match <- NULL
    split(rdf, sentence_group)
  }
})

rd_df_recalc_whitespace <- function(rdf) {
  rdf$is_whitespace[rdf$is_text] <- grepl("^\\s*$", rdf$content[rdf$is_text])
  rdf
}

rd_df_collapse_text <- function(rdf) {
  collapsible <- rdf$tag == "TEXT" &
    apply(rdf[, c("is_text", "lintable"), drop = FALSE], 1, all)
  group <- cumsum(c(TRUE, head(collapsible, -1) != tail(collapsible, -1)))
  rdf <- do.call(rbind, lapply(split(rdf, group), function(rdf) {
    if (rdf$tag[[1]] != "TEXT") return(rdf)
    rdf$content[[1]] <- paste(rdf$content, collapse = "")
    rdf[1, , drop = FALSE]
  }))
  rdf <- rd_df_recalc_whitespace(rdf)
  rdf
}

rd_df_expand_allow_list <- function(rdf, allowed = NULL) {
  if (is.null(allowed)) return(rdf)

  # escape regex, replace any whitespace with arbitrary whitespace regex
  patterns <- gsub("\\s+", "\\\\s+", vcapply(allowed, re_escape))
  re <- paste0(patterns, collapse = "|")

  # expand matches and tag them as non-lintable
  rdf <- rd_df_expand_content_re(rdf, re)
  rdf$lintable <- rdf$lintable & !rdf$is_match
  rdf$is_match <- NULL
  rdf
}

rd_df_expand_words <- function(rdf) {
  rdf <- rd_df_expand_content_re(rdf, "\\s+")
  rdf <- rdf[!rdf$is_match, , drop = FALSE]
  rdf$is_match <- NULL
  rdf <- rd_df_recalc_whitespace(rdf)
  rdf
}

rd_df_expand_content_re <- function(rdf, re, ..., which = "both") {
  m <- gregexec(re, rdf$content, ...)
  splits <- split_string_matches(rdf$content, m, which = which)
  idx <- rep(seq_along(splits), times = vapply(splits, length, integer(1L)))
  rdf <- rdf[idx, ]
  rdf$content <- unlist(splits)
  rdf$is_match <- switch(which,
    "both" = unlist(lapply(splits, seq_along)) %% 2 == 0,
    "left" = unlist(lapply(splits, seq_along)) != 1,
    "right" = unlist(lapply(splits, function(i) rev(seq_along(i)))) != 1
  )
  rdf
}

split_string_matches <- function(x, m, which = c("both", "left", "right")) {
  which <- match.arg(which)
  mapply(
    function(x, match) {
      if (identical(match[[1]], -1L)) return(x)
      m_start <- match
      m_len <- attr(match, "match.length")

      cuts <- switch(which,
        "both" = as.vector(rbind(m_start, m_start + m_len)),
        "left" = m_start,
        "right" = m_start + m_len
      )

      substring(x, c(0, cuts), c(cuts - 1, nchar(x)))
    },
    x, m,
    SIMPLIFY = FALSE,
    USE.NAMES = FALSE
  )
}

split_string_at <- function(x, at) {
  mapply(
    function(x, at) {
      if (identical(at, -1L)) return(x)
      substring(x, c(0, at + 1), c(at, nchar(x)))
    },
    x, at,
    SIMPLIFY = FALSE,
    USE.NAMES = FALSE
  )
}

rd_df_check_sentence_head <- function(rdf) {
  # first non-whitespace row
  idx <- Position(identity, !rdf$is_whitespace)
  if (is.na(idx)) return()

  # check if first token is text, lintable and starts capitalized
  is_cap <- rdf$is_text[[idx]] &&
    rdf$lintable[[idx]] &&
    grepl("\\s*([[:upper:]]|[^[:alpha:]])", rdf$content[[idx]])

  if (!is_cap) rdf$content[[idx]]
}

rd_df_check_sentence_tail <- function(rdf) {
  # first non-whitespace row
  idx <- Position(identity, !rdf$is_whitespace)
  if (is.na(idx)) return()

  rdf <- utils::tail(rdf, -idx)
  rdf[rdf$is_text,]
}
