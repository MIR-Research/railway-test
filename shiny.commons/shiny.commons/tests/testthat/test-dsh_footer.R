test_that("dsh_footer() produces a footer that is tagged with shiny", {
  foot <- dsh_footer()
  expect_true(inherits(foot, "shiny.tag"))
})

test_that("dsh_footer() produces a footer that is a named div", {
  foot <- dsh_footer()
  expect_equal(foot$name, "div")
})

test_that("dsh_footer() produces a footer with the 'footer' class name", {
  foot <- dsh_footer()
  expect_equal(foot$attribs$class, "footer")
})
