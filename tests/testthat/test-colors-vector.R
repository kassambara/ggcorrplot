fill_scale <- function(g) g$scales$get_scales("fill")

n_fill_scales <- function(g) {
  sum(vapply(g$scales$scales, function(s) "fill" %in% s$aesthetics, logical(1)))
}

test_that("the default (length-3) colors still use scale_fill_gradient2", {
  corr <- round(cor(mtcars), 1)
  g <- ggcorrplot(corr)
  expect_equal(n_fill_scales(g), 1L)
  cl <- fill_scale(g)$call
  if (!is.null(cl)) {
    expect_match(paste(deparse(cl), collapse = " "), "gradient2")
  }
})

test_that("an explicit length-3 colors vector keeps the gradient2 path", {
  corr <- round(cor(mtcars), 1)
  g <- ggcorrplot(corr, colors = c("#6D9EC1", "white", "#E46726"))
  cl <- fill_scale(g)$call
  if (!is.null(cl)) {
    expect_match(paste(deparse(cl), collapse = " "), "gradient2")
  }
})

test_that("a colors vector of length != 3 uses gradientn and one fill scale", {
  corr <- round(cor(mtcars), 1)
  pal <- c("#67001F", "#D6604D", "#F7F7F7", "#4393C3", "#053061") # length 5
  g <- ggcorrplot(corr, colors = pal)
  # exactly one fill scale: the palette is applied without adding a second one
  expect_equal(n_fill_scales(g), 1L)
  cl <- fill_scale(g)$call
  if (!is.null(cl)) {
    expect_match(paste(deparse(cl), collapse = " "), "gradientn")
  }
  # builds without error (a longer palette used to require a manual second scale)
  expect_silent(ggplot2::ggplot_build(g))
})

test_that("a length-2 colors vector also works via gradientn", {
  corr <- round(cor(mtcars), 1)
  g <- ggcorrplot(corr, colors = c("navy", "gold"))
  expect_equal(n_fill_scales(g), 1L)
  cl <- fill_scale(g)$call
  if (!is.null(cl)) {
    expect_match(paste(deparse(cl), collapse = " "), "gradientn")
  }
})

test_that("fewer than 2 colors gives a clear error", {
  corr <- round(cor(mtcars), 1)
  expect_error(ggcorrplot(corr, colors = "red"), "at least 2")
})

test_that("legend.limit is honored on the gradientn path", {
  corr <- round(cor(mtcars), 1)
  pal <- c("#67001F", "#D6604D", "#F7F7F7", "#4393C3", "#053061")
  g <- ggcorrplot(corr, colors = pal, legend.limit = c(-1, 1))
  expect_equal(fill_scale(g)$limits, c(-1, 1))
})
