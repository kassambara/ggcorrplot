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

test_that("hc.order is honored for numeric-looking dimnames and as.is (#37)", {
  # reshape2::melt used to type-convert numeric-looking names to a continuous
  # axis sorted by value (and as.is = TRUE to an alphabetical character axis),
  # silently discarding hc.order. The axis is now coerced to a factor in
  # cluster order.
  m <- round(cor(mtcars), 1)
  ord <- function(x) stats::hclust(stats::as.dist((1 - x) / 2))$order

  mn <- m
  dimnames(mn) <- list(as.character(seq_len(ncol(m))), as.character(seq_len(ncol(m))))
  g <- ggcorrplot(mn, hc.order = TRUE)
  expect_s3_class(g$data$Var1, "factor")
  expect_identical(levels(g$data$Var1), as.character(seq_len(ncol(m)))[ord(mn)])
  # not the numeric value order
  expect_false(identical(levels(g$data$Var1), as.character(seq_len(ncol(m)))))

  # as.is = TRUE on ordinary names also follows the clustering, not the alphabet
  ga <- ggcorrplot(m, hc.order = TRUE, as.is = TRUE)
  expect_identical(levels(ga$data$Var1), rownames(m)[ord(m)])
})

test_that("hc.order works for non-canonical numeric names, not just '1'..'n' (#37)", {
  m <- round(cor(mtcars[, 1:5]), 1)
  ord <- function(x) stats::hclust(stats::as.dist((1 - x) / 2))$order
  # names whose string form is not their canonical numeric string: melt would
  # type-convert "01" -> 1 -> "1", so a naive coercion would produce all-NA cells
  for (nms in list(
    c("01", "02", "03", "04", "05"),
    c("1.10", "2.20", "3.30", "4.40", "5.50"),
    c("1e2", "2e2", "3e2", "4e2", "5e2")
  )) {
    mm <- m
    dimnames(mm) <- list(nms, nms)
    d <- ggcorrplot(mm, hc.order = TRUE)$data
    expect_false(anyNA(d$Var1))
    expect_false(anyNA(d$Var2))
    expect_identical(levels(d$Var1), nms[ord(mm)])
  }
})

test_that("duplicated variable names do not crash the plot", {
  m <- round(cor(mtcars[, 1:4]), 1)
  dimnames(m) <- list(c("a", "a", "b", "c"), c("a", "a", "b", "c"))
  expect_s3_class(ggplot2::ggplotGrob(ggcorrplot(m)), "gtable")
})
