test_tag <- function(..., markdown = TRUE) {
  if (markdown) roxygen2:::local_markdown()
  roxygen2::roxy_tag_parse(roxygen2::roxy_tag(...))
}
