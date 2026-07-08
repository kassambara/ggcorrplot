test_that("cor_pmat default use is unchanged (byte-identical)", {
  # the default 'pairwise.complete.obs' must reproduce the historical output
  expect_identical(cor_pmat(mtcars), cor_pmat(mtcars, use = "pairwise.complete.obs"))
  expect_false(anyNA(cor_pmat(mtcars)))
  # partial-NA but every pair still computable: default must not blank anything new
  set.seed(1)
  m <- mtcars
  m[cbind(sample(32, 6), sample(11, 6))] <- NA
  pm <- cor_pmat(m)
  expect_equal(pm, t(pm))
})

test_that("use = 'everything' aligns the NA pattern with cor()'s default", {
  d <- data.frame(
    A = c(1, 2, 3, 4, 5, NA, NA, NA, NA, NA),
    B = c(2, 4, 6, 8, 10, 1, 2, 3, 4, 5),
    C = c(5, 3, 6, 2, 8, 1, 9, 4, 7, 0),
    D = c(NA, NA, NA, NA, NA, 5, 4, 3, 2, 1)
  )
  # cor() default is use = "everything": NA wherever a column has any NA
  expect_identical(is.na(cor_pmat(d, use = "everything")), is.na(cor(d)))
  # pairs where both columns are complete still get a real p-value
  expect_false(is.na(cor_pmat(d, use = "everything")["B", "C"]))
  # symmetric, zero diagonal preserved
  pm <- cor_pmat(d, use = "everything")
  expect_equal(pm, t(pm))
  expect_equal(diag(pm), rep(0, 4), ignore_attr = TRUE)
})

test_that("use is restricted to the two safe values", {
  d <- data.frame(a = c(1, 2, 3, NA), b = c(4, 3, 2, 1), c = c(1, 1, 2, 3))
  # listwise options would make the p-value and the coefficient disagree; rejected
  expect_error(cor_pmat(d, use = "complete.obs"))
  expect_error(cor_pmat(d, use = "na.or.complete"))
  # all.obs would only ever error on NA data; rejected
  expect_error(cor_pmat(d, use = "all.obs"))
})

test_that("the uncomputable-pair fix still holds under the default use", {
  # two variables that never co-occur -> NA, no crash (regression guard for #51/#78)
  d <- data.frame(
    A = c(1, 2, 3, 4, 5, NA, NA, NA, NA, NA),
    B = c(2, 4, 6, 8, 10, 1, 2, 3, 4, 5),
    D = c(NA, NA, NA, NA, NA, 5, 4, 3, 2, 1)
  )
  pm <- expect_no_error(cor_pmat(d))
  expect_true(is.na(pm["A", "D"]))
})
