# Regression tests for the tl.col argument (#44)

test_that("tl.col colours the axis text on both axes (#44)", {
  corr <- round(cor(mtcars), 1)
  g <- ggcorrplot(corr, tl.col = "red")
  expect_equal(ggplot2::calc_element("axis.text.x", g$theme)$colour, "red")
  expect_equal(ggplot2::calc_element("axis.text.y", g$theme)$colour, "red")
})

test_that("tl.col defaults to inheriting the theme colour, not a forced black (#44 no-regression)", {
  corr <- round(cor(mtcars), 1)
  default_col <- ggplot2::calc_element("axis.text.x", ggcorrplot(corr)$theme)$colour
  theme_col   <- ggplot2::calc_element("axis.text", ggplot2::theme_minimal())$colour
  expect_equal(default_col, theme_col)
})
