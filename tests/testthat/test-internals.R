# Contract of the internal builders extracted from ggcorrplot() (P0.0).
# These helpers are unexported, but the mixed-method and ellipse paths are built
# on them, so their return shape is a real contract worth pinning. The behavior
# they produce is covered byte-for-byte by test-structure.R (the plot is
# unchanged by the extraction); here we lock the shape of what they return.

corr <- round(cor(mtcars), 1)
p.mat <- cor_pmat(mtcars)
n <- ncol(mtcars)

test_that(".build_corr_df returns the melted corr frame and a NULL p.mat when none is given", {
  b <- .build_corr_df(
    corr = corr, p.mat = NULL, type = "full", show.diag = TRUE,
    hc.order = FALSE, hc.method = "complete", digits = 2,
    sig.level = 0.05, insig = "pch", as.is = FALSE
  )
  expect_named(b, c("corr", "p.mat"))
  expect_s3_class(b$corr, "data.frame")
  expect_null(b$p.mat)
  expect_true(all(c("Var1", "Var2", "value", "pvalue", "signif", "abs_corr") %in% names(b$corr)))
  expect_equal(nrow(b$corr), n * n)
  # with no p.mat, significance columns are all NA
  expect_true(all(is.na(b$corr$pvalue)))
})

test_that(".build_corr_df joins p-values and returns the insignificant-cells frame", {
  b <- .build_corr_df(
    corr = corr, p.mat = p.mat, type = "full", show.diag = TRUE,
    hc.order = FALSE, hc.method = "complete", digits = 2,
    sig.level = 0.05, insig = "pch", as.is = FALSE
  )
  expect_s3_class(b$p.mat, "data.frame")
  # the p.mat frame holds exactly the cells with p > sig.level
  expect_equal(nrow(b$p.mat), sum(p.mat > 0.05))
  # every corr cell's pvalue is its own (Var1, Var2) pair's p-value
  expected <- p.mat[cbind(as.character(b$corr$Var1), as.character(b$corr$Var2))]
  expect_equal(b$corr$pvalue, unname(expected))
})

test_that(".build_corr_df applies the triangle mask via row count", {
  tri <- n * (n - 1) / 2
  lower <- .build_corr_df(
    corr = corr, p.mat = NULL, type = "lower", show.diag = FALSE,
    hc.order = FALSE, hc.method = "complete", digits = 2,
    sig.level = 0.05, insig = "pch", as.is = FALSE
  )
  expect_equal(nrow(lower$corr), tri)
})

test_that(".build_corr_df errors on a non-square matrix with hc.order or a triangle", {
  ns <- cor(mtcars)[1:3, 1:5]
  expect_error(
    .build_corr_df(ns, NULL, "full", TRUE, TRUE, "complete", 2, 0.05, "pch", FALSE),
    "square"
  )
  expect_error(
    .build_corr_df(ns, NULL, "lower", FALSE, FALSE, "complete", 2, 0.05, "pch", FALSE),
    "square"
  )
})

test_that(".method_layer returns a single tile layer for square", {
  layers <- .method_layer("square", outline.color = "gray", circle.scale = 1)
  expect_type(layers, "list")
  expect_length(layers, 1)
  expect_s3_class(layers[[1]], "LayerInstance")
  expect_s3_class(layers[[1]]$geom, "GeomTile")
})

test_that(".method_layer returns a point layer plus size scale and guide for circle", {
  layers <- .method_layer("circle", outline.color = "gray", circle.scale = 1)
  expect_length(layers, 3)
  expect_s3_class(layers[[1]], "LayerInstance")
  expect_s3_class(layers[[1]]$geom, "GeomPoint")
  expect_s3_class(layers[[2]], "Scale")
  # the layers add cleanly onto a ggplot (the caller does p + .method_layer(...))
  d <- .build_corr_df(corr, NULL, "full", TRUE, FALSE, "complete", 2, 0.05, "pch", FALSE)$corr
  p <- ggplot2::ggplot(d, ggplot2::aes(Var1, Var2, fill = value)) + layers
  expect_s3_class(ggplot2::ggplotGrob(p), "gtable")
})
