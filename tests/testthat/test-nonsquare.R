# Non-square correlation matrix handling (#5, #10)

test_that("non-square matrix plots in full but errors for clustered/triangular layouts", {
  rect <- cor(mtcars[, 1:3], mtcars[, 4:7])   # 3 x 4

  # type = "full" (the default) works for a non-square matrix
  expect_no_error(ggplot2::ggplot_build(ggcorrplot(rect)))

  # hc.order and the triangular layouts require a square matrix -> clear error
  expect_error(ggcorrplot(rect, hc.order = TRUE), "square")
  expect_error(ggcorrplot(rect, type = "lower"), "square")
  expect_error(ggcorrplot(rect, type = "upper"), "square")
})

test_that("square matrices are unaffected by the non-square guard", {
  sq <- round(cor(mtcars), 1)
  expect_no_error(ggplot2::ggplot_build(ggcorrplot(sq, hc.order = TRUE, type = "lower")))
})
