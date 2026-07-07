# Regression tests for p-value matrix handling (#25, #37)

# Count the significance-marker (pch = 4 cross) glyphs in a built plot.
.n_crosses <- function(p) {
  b <- ggplot2::ggplot_build(p)$data
  cl <- Filter(function(d) "shape" %in% names(d) && nrow(d) > 0 && all(d$shape == 4), b)
  if (length(cl)) nrow(cl[[1]]) else 0L
}

test_that("hc.order does not alter significance via p-value rounding (#25)", {
  cmat <- matrix(c(1, 0.3, 0.2, 0.3, 1, 0.25, 0.2, 0.25, 1), 3,
                 dimnames = list(letters[1:3], letters[1:3]))
  # p = 0.054 is just ABOVE the default sig.level (0.05); it must stay
  # non-significant (a cross is drawn) whether or not hc.order is used.
  pmat <- matrix(c(0, 0.054, 0.20, 0.054, 0, 0.30, 0.20, 0.30, 0), 3,
                 dimnames = list(letters[1:3], letters[1:3]))
  n_no_hc <- .n_crosses(ggcorrplot(cmat, p.mat = pmat, insig = "pch"))
  n_hc    <- .n_crosses(ggcorrplot(cmat, p.mat = pmat, hc.order = TRUE, insig = "pch"))
  # all 6 off-diagonal cells are non-significant -> 6 crosses either way
  expect_equal(n_no_hc, 6L)
  expect_equal(n_hc, n_no_hc)
})

test_that("hc.order leaves significance unchanged for non-boundary p (#25 no-regression)", {
  corr <- round(cor(mtcars), 1)
  p    <- cor_pmat(mtcars)
  expect_equal(
    .n_crosses(ggcorrplot(corr, p.mat = p, hc.order = TRUE,  insig = "pch")),
    .n_crosses(ggcorrplot(corr, p.mat = p, hc.order = FALSE, insig = "pch"))
  )
})

test_that("p-values stay aligned when corr and p.mat NA patterns differ", {
  c4 <- round(cor(mtcars[, 1:4]), 2)
  p4 <- cor_pmat(mtcars[, 1:4])
  # remove one pair from the p-value matrix only -> different NA pattern
  p4["mpg", "disp"] <- p4["disp", "mpg"] <- NA
  # previously this raised "replacement has 14 rows, data has 16"
  expect_no_error(ggcorrplot(c4, p.mat = p4, insig = "pch"))
  # blank mode must not error either (unknown-significance cells are kept)
  expect_no_error(ggcorrplot(c4, p.mat = p4, insig = "blank"))
  expect_no_error(ggcorrplot(c4, p.mat = p4, insig = "blank", lab = TRUE))
})

test_that("significance markers are matched to cells by name, not position", {
  m <- matrix(c(1, 0.9, 0.2, 0.9, 1, 0.1, 0.2, 0.1, 1), 3,
              dimnames = list(c("a", "b", "c"), c("a", "b", "c")))
  # only the a~c pair is non-significant
  p <- matrix(c(0, 0.001, 0.6, 0.001, 0, 0.002, 0.6, 0.002, 0), 3,
              dimnames = list(c("a", "b", "c"), c("a", "b", "c")))
  cross <- .n_crosses(ggcorrplot(m, p.mat = p, insig = "pch"))
  expect_equal(cross, 2L)   # exactly a~c and c~a
})

test_that("p.mat aligns with numeric-looking names when as.is = TRUE (#37)", {
  nm <- c("10", "2", "33")
  m  <- matrix(c(1, 0.8, 0.2, 0.8, 1, 0.1, 0.2, 0.1, 1), 3, dimnames = list(nm, nm))
  pm <- matrix(c(0, 0.6, 0.9, 0.6, 0, 0.7, 0.9, 0.7, 0), 3, dimnames = list(nm, nm))
  b  <- ggplot2::ggplot_build(ggcorrplot(m, p.mat = pm, as.is = TRUE, insig = "pch"))$data
  cl <- Filter(function(d) "shape" %in% names(d) && nrow(d) > 0 && all(d$shape == 4), b)
  expect_true(length(cl) > 0)
  # crosses must land on the discrete tile positions (1..3), not the literal
  # names (10, 33) that would otherwise place them off-plot.
  expect_true(all(cl[[1]]$x %in% seq_len(3)))
  expect_true(all(cl[[1]]$y %in% seq_len(3)))
})
