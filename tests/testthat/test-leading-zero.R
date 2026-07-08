text_labels <- function(g) {
  i <- which(vapply(g$layers, function(L) inherits(L$geom, "GeomText"), logical(1)))
  g$layers[[i]]$aes_params$label
}

test_that("leading.zero defaults to keeping the zero (unchanged behavior)", {
  corr <- round(cor(mtcars), 2)
  labs <- text_labels(ggcorrplot(corr, lab = TRUE))
  # at least one label like 0.xx or -0.xx present, none stripped
  expect_true(any(grepl("^-?0\\.", labs)))
  expect_false(any(grepl("^-?\\.[0-9]", labs)))
})

test_that("leading.zero = FALSE drops the leading zero, including negatives", {
  corr <- round(cor(mtcars), 2)
  labs <- text_labels(ggcorrplot(corr, lab = TRUE, leading.zero = FALSE))
  # no label should keep a leading 0 before the decimal
  expect_false(any(grepl("^-?0\\.", labs)))
  # values in (-1, 1) now start with . or -.
  expect_true(any(grepl("^\\.[0-9]", labs)))   # e.g. .68
  expect_true(any(grepl("^-\\.[0-9]", labs)))  # e.g. -.85
})

test_that("leading.zero = FALSE leaves 1.00 and integers untouched", {
  m <- matrix(c(1, -0.67, 0.23, 1), 2, 2,
    dimnames = list(c("x", "y"), c("x", "y")))
  labs <- text_labels(ggcorrplot(m, lab = TRUE, nsmall = 2, leading.zero = FALSE))
  expect_true("1.00" %in% labs)          # the 0 in 1.00 is not a leading zero
  expect_true("-.67" %in% labs)
  expect_true(".23" %in% labs)
  expect_false(any(labs == "1.0.0"))     # sanity: no double substitution
})

test_that("leading.zero = FALSE composes with sig.stars", {
  corr <- round(cor(mtcars), 2)
  p <- cor_pmat(mtcars)
  labs <- text_labels(
    ggcorrplot(corr, p.mat = p, lab = TRUE, sig.stars = TRUE, leading.zero = FALSE)
  )
  # a starred, zero-stripped label such as -.85*** exists, and none keep 0.
  expect_true(any(grepl("^-?\\.[0-9]+\\*", labs)))
  expect_false(any(grepl("^-?0\\.", labs)))
})
