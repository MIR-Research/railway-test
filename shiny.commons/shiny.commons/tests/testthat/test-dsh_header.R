test_that("dsh_header() produces a header that is tagged with shiny", {
  header <- dsh_header("title txt", "window txt")
  expect_true(inherits(header[[1]], "shiny.tag"))
})

test_that("dsh_header() produces a header with 593 characters in the style section", {
  header <- dsh_header("title txt", "window txt")
  style <- nchar(header[[1]]$children)

  expect_equal(style, 1200)
})
