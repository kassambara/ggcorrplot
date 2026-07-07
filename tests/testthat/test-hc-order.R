# Regression test for hc.order clustering on the unrounded matrix (#14)

test_that("hc.order clusters on the unrounded matrix, not the rounded one (#14)", {
  m <- structure(
    c(1, 0.6063, 0.1987, 0.2749, -0.2482, 0.231,
      0.6063, 1, -0.0788, 0.2389, -0.0252, 0.4772,
      0.1987, -0.0788, 1, 0.7596, 0.3316, -0.445,
      0.2749, 0.2389, 0.7596, 1, 0.8052, 0.5687,
      -0.2482, -0.0252, 0.3316, 0.8052, 1, -0.144,
      0.231, 0.4772, -0.445, 0.5687, -0.144, 1),
    dim = c(6L, 6L),
    dimnames = list(LETTERS[1:6], LETTERS[1:6])
  )
  ord_of <- function(x) stats::hclust(stats::as.dist((1 - x) / 2), method = "complete")$order

  # For this matrix, rounding to 1 digit changes the clustering order.
  expect_false(identical(ord_of(m), ord_of(round(m, 1))))

  # ggcorrplot() must order by the UNROUNDED matrix even with an aggressive
  # digits setting (previously it rounded first, so digits perturbed the order).
  g <- ggcorrplot(m, hc.order = TRUE, digits = 1)
  expect_equal(levels(g$data$Var1), rownames(m)[ord_of(m)])
})
