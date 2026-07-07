# Tests for the circle.scale argument (#8)

test_that("circle.scale scales the circle sizes; default is unchanged (#8)", {
  corr <- round(cor(mtcars), 1)
  size_range <- function(cs) {
    b <- ggplot2::ggplot_build(ggcorrplot(corr, method = "circle", circle.scale = cs))$data
    pts <- Filter(function(d) "size" %in% names(d) && nrow(d) > 0 && all(d$shape == 21), b)
    range(pts[[1]]$size)
  }
  # default range is c(4, 10); circle.scale multiplies it uniformly
  expect_equal(size_range(1), c(4, 10))
  expect_equal(size_range(2), c(8, 20))
})

test_that("circle.scale has no effect on method = 'square' (#8)", {
  corr <- round(cor(mtcars), 1)
  b1 <- ggplot2::ggplot_build(ggcorrplot(corr, circle.scale = 1))$data
  b3 <- ggplot2::ggplot_build(ggcorrplot(corr, circle.scale = 3))$data
  expect_identical(b1, b3)
})
