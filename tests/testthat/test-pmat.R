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
