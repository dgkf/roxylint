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

  rd_df(items)
}

#' @export
rd_df.item <- function(rd) {
  if (length(rd) == 1) rd_df(rd[[-1]]) else rd_df(rd[-1])
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

