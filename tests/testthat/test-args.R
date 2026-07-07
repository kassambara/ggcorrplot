# Tests for the nsmall (#43) and legend.limit (#54) arguments

.text_labels <- function(g) {
  i <- which(vapply(g$layers, function(l) inherits(l$geom, "GeomText"), logical(1)))
  g$layers[[i[1]]]$aes_params$label
}

test_that("nsmall keeps trailing zeros in labels; default is unchanged (#43)", {
  corr <- round(cor(mtcars), 1)
  d0 <- .text_labels(ggcorrplot(corr, lab = TRUE))              # default nsmall = 0
  d2 <- .text_labels(ggcorrplot(corr, lab = TRUE, nsmall = 2))  # two decimals
  # nsmall = 2 formats to exactly two decimals (e.g. 0.70, 1.00)
  expect_type(d2, "character")
  expect_true(all(grepl("^-?[0-9]+\\.[0-9]{2}$", d2)))
  expect_true(any(d2 %in% c("1.00", "0.70", "-0.70", "-0.90")))
  # default keeps the plain rounded values (no forced trailing zeros)
  expect_false(any(as.character(d0) %in% c("1.00", "0.70")))
})

test_that("legend.limit controls the fill scale range; default is unchanged (#54)", {
  cv <- cov(mtcars)
  grey <- function(...) {
    sum(ggplot2::ggplot_build(ggcorrplot(cv, ...))$data[[1]]$fill == "grey50")
  }
  # default limit c(-1, 1) clips covariance values out of range -> grey cells
  expect_gt(grey(), 0)
  # NULL uses the data range -> nothing clipped
  expect_equal(grey(legend.limit = NULL), 0L)
})
