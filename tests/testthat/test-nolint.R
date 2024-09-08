test_that("blanket #nolint declarations suppress all lints", {
  blanket_nolint_tag <- test_tag("title", "**Not** [www.test.com](Sentence) Case `r # nolint`")
  expect_silent(lint_sentence_case(blanket_nolint_tag))
})

test_that("qualified #nolint declarations suppress only itemized lints", {
  qualified_nolint_tag <- test_tag(
    "title",
    "Not Sentence Case `r # nolint: sentence_case`"
  )

  expect_silent(lint_sentence_case(qualified_nolint_tag))

  qualified_but_wrong_nolint_tag <- test_tag(
    "title",
    "Not Sentence Case `r # nolint: title_case`"
  )

  expect_message(lint_sentence_case(qualified_but_wrong_nolint_tag))
})

test_that("qualified #nolint declarations can be puncutated", {
  qualified_nolint_tag <- roxygen2::roxy_tag(
    "qualified-nolint",
    "Not sentence Case Or title Case `r # nolint: sentence_case,title_case.`"
  )

  expect_silent(lint_sentence_case(qualified_nolint_tag))
  expect_silent(lint_title_case(qualified_nolint_tag))
})

test_that("qualified #nolint declarations parsed from multiline code", {
  qualified_nolint_tag <- roxygen2::roxy_tag(
    "qualified-nolint",
    "
    Not sentence Case Or title `r #
    nolint sentence_case title_case` Case
    "
  )

  expect_silent(lint_sentence_case(qualified_nolint_tag))
  expect_silent(lint_title_case(qualified_nolint_tag))
})
