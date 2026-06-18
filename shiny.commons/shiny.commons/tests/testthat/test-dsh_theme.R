test_that("DSH theme applies to ggplot object", {
  plt <- ggplot2::ggplot(data = df, mapping = ggplot2::aes(yield, fill = crop)) +
    ggplot2::geom_density() +
    theme_dsh()

  expect_identical(theme_dsh(), plt$theme)
})


test_that("DSH scale fill applies to ggplot object", {
  plt <- ggplot2::ggplot(data = df, mapping = ggplot2::aes(yield, fill = crop)) +
    ggplot2::geom_density() +
    scale_fill_dsh() +
    theme_dsh()

  plt_fill <- ggplot2::ggplot_build(plt)
  fill_values <- plt_fill$data[[1]]$fill

  expect_true(all(fill_values %in% usda_palette))
})


test_that("DSH scale color applies to ggplot object", {
  plt <- ggplot2::ggplot(data = df, mapping = ggplot2::aes(yield, color = crop)) +
    ggplot2::geom_density() +
    scale_color_dsh() +
    theme_dsh()

  plt_color <- ggplot2::ggplot_build(plt)
  color_values <- plt_color$data[[1]]$color

  expect_true(all(color_values %in% usda_palette))
})
