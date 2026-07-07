test_that("cor_pmat on complete data is unchanged (no NA, symmetric, zero diag)", {
  pm <- cor_pmat(mtcars)
  expect_false(anyNA(pm))
  expect_equal(diag(pm), rep(0, ncol(mtcars)), ignore_attr = TRUE)
  expect_equal(pm, t(pm))
})

test_that("cor_pmat returns NA for an uncomputable pair instead of erroring", {
  # A and D never co-occur (complementary missingness) -> no overlapping obs
  d <- data.frame(
    A = c(1, 2, 3, 4, 5, NA, NA, NA, NA, NA),
    B = c(2, 4, 6, 8, 10, 1, 2, 3, 4, 5),
    C = c(5, 3, 6, 2, 8, 1, 9, 4, 7, 0),
    D = c(NA, NA, NA, NA, NA, 5, 4, 3, 2, 1)
  )
  pm <- expect_no_error(cor_pmat(d))
  expect_true(is.na(pm["A", "D"]))
  expect_true(is.na(pm["D", "A"]))
  # computable pairs still get a p-value
  expect_false(is.na(pm["B", "C"]))
  # diagonal and symmetry preserved
  expect_equal(diag(pm), rep(0, 4), ignore_attr = TRUE)
  expect_equal(pm, t(pm))
})

test_that("cor_pmat does not swallow genuine errors into NA", {
  # a bad method= must still error loudly, not silently return an all-NA matrix
  expect_error(cor_pmat(mtcars, method = "bogus"))
  # a non-numeric column must still error, not become NA
  dd <- data.frame(a = 1:10, b = letters[1:10], c = rnorm(10))
  expect_error(cor_pmat(dd))
})

test_that("cor_pmat still forwards extra args to cor.test", {
  pearson <- cor_pmat(mtcars)
  # method = "spearman" is forwarded to cor.test (ties warning is expected on
  # mtcars and not what we are testing here).
  spearman <- suppressWarnings(cor_pmat(mtcars, method = "spearman"))
  expect_false(anyNA(spearman))
  expect_equal(spearman, t(spearman))
  # forwarding actually took effect: spearman p-values differ from pearson
  expect_false(isTRUE(all.equal(pearson, spearman)))
})
