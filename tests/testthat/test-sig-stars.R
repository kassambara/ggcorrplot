# Tests for the sig.stars argument (#26, #41, #50)

.text_labels <- function(g) {
  i <- which(vapply(g$layers, function(l) inherits(l$geom, "GeomText"), logical(1)))
  g$layers[[i[1]]]$aes_params$label
}
.n_crosses <- function(g) {
  b <- ggplot2::ggplot_build(g)$data
  sum(vapply(b, function(d) {
    if ("shape" %in% names(d) && nrow(d) > 0 && all(d$shape == 4)) nrow(d) else 0L
  }, 0L))
}

test_that("sig.stars appends significance stars to labels; default off is unchanged (#26, #41)", {
  corr <- round(cor(mtcars), 1)
  p <- cor_pmat(mtcars)

  # default: no stars appended
  d0 <- .text_labels(ggcorrplot(corr, p.mat = p, lab = TRUE))
  expect_false(any(grepl("[*]", d0)))

  # sig.stars = TRUE: stars on significant cells, bare number on non-significant
  d1 <- .text_labels(ggcorrplot(corr, p.mat = p, lab = TRUE, sig.stars = TRUE))
  expect_true(any(grepl("[*]{3}$", d1)))          # p < 0.001
  expect_true(any(grepl("(?<![*])[*]$", d1, perl = TRUE)))  # single star
  expect_true(any(grepl("^-?[0-9.]+$", d1)))      # non-significant: bare number
})

test_that("sig.stars suppresses the insig = 'pch' crosses (#33 stays available by default)", {
  corr <- round(cor(mtcars), 1)
  p <- cor_pmat(mtcars)
  # default (pch) draws crosses; sig.stars conveys significance instead
  expect_gt(.n_crosses(ggcorrplot(corr, p.mat = p, lab = TRUE)), 0)
  expect_equal(.n_crosses(ggcorrplot(corr, p.mat = p, lab = TRUE, sig.stars = TRUE)), 0)
})
