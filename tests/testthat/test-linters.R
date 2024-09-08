test_that("title_case linter works", {
  title_tag <- roxygen2::roxy_tag_parse(roxygen2::roxy_tag(
    "title",
    "Create a [roxygen2::roxy_tag()] Tag"  # ensure links don't trigger lints
  ))

  expect_silent(
    lint_title_case(title_tag)
  )
})

test_that("sentence_case linter works", {
  clean_tag <- roxygen2::roxy_tag("clean", "Is sentence case")
  expect_silent(
    lint_sentence_case(clean_tag)
  )

  linty_tag <- roxygen2::roxy_tag("linty", "Not Sentence Case")
  expect_message(
    lint_sentence_case(linty_tag),
    "[Ss]entence [Cc]ase"
  )
})
