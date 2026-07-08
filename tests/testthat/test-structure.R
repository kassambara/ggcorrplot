# Structural contract of the plot ggcorrplot() builds.
#
# The vdiffr snapshots in test-vdiffr.R are skip_on_ci() and skip_on_cran(), so
# they never run outside a maintainer's laptop. The assertions below are the
# cross-cutting safety net that DOES run on CI and CRAN: layer composition,
# built-data shape, the melt contract, scale semantics, and the coordinate
# system. Individual arguments are covered by their own test files; this file
# locks in how the pieces fit together, which is what a refactor of the
# transform-then-plot pipeline is liable to break.
#
# Values here were read off the current implementation, not assumed. Where a
# number is derived (e.g. 11 * 11 cells), it is written as the expression.

corr <- round(cor(mtcars), 1)
p.mat <- cor_pmat(mtcars)
n <- ncol(mtcars) # 11

geoms <- function(p) unname(vapply(p$layers, function(l) class(l$geom)[1], character(1)))
layer_rows <- function(p) unname(vapply(ggplot2::ggplot_build(p)$data, nrow, integer(1)))
fill_scale <- function(p) {
  s <- p$scales$scales
  i <- which(vapply(s, function(x) "fill" %in% x$aesthetics, logical(1)))
  if (length(i) != 1L) stop("expected exactly one fill scale, found ", length(i))
  s[[i]]
}


# ---- layer composition -------------------------------------------------------

test_that("the default plot is a single tile layer over every cell", {
  p <- ggcorrplot(corr)
  expect_equal(geoms(p), "GeomTile")
  expect_equal(layer_rows(p), n * n)
})

test_that("positional arguments still bind as before (new args are appended, not inserted)", {
  # the mixed-layout args were added at the END of the signature, so a positional
  # `type` call must keep working: ggcorrplot(corr, <method>, <type>)
  p <- ggcorrplot(corr, "circle", "lower")
  expect_equal(geoms(p), "GeomPoint")
  expect_equal(layer_rows(p), n * (n - 1) / 2) # lower triangle, off-diagonal
  full <- ggcorrplot(corr, "circle", "full")
  expect_equal(layer_rows(full), n * n)
})

test_that("method = 'circle' swaps the tile layer for a sized point layer", {
  p <- ggcorrplot(corr, method = "circle")
  expect_equal(geoms(p), "GeomPoint")
  expect_equal(layer_rows(p), n * n)
  # shape 21 is what makes the point fillable and outline-able
  expect_identical(p$layers[[1]]$aes_params$shape, 21)
  # size is mapped, and its legend suppressed
  expect_true("size" %in% names(p$layers[[1]]$mapping))
})

test_that("lab = TRUE adds a text layer on top of the glyph layer, in that order", {
  p <- ggcorrplot(corr, lab = TRUE)
  expect_equal(geoms(p), c("GeomTile", "GeomText"))
  expect_equal(layer_rows(p), c(n * n, n * n))
})

test_that("insig = 'pch' adds a point layer holding exactly the non-significant cells", {
  p <- ggcorrplot(corr, p.mat = p.mat)
  expect_equal(geoms(p), c("GeomTile", "GeomPoint"))
  # the pch layer marks p > sig.level, and nothing else
  expect_equal(layer_rows(p)[2], sum(p.mat > 0.05))
})

test_that("insig = 'blank' adds no layer -- it zeroes the fill value instead", {
  p <- ggcorrplot(corr, p.mat = p.mat, insig = "blank")
  expect_equal(geoms(p), "GeomTile")
  expect_equal(sum(p$data$value == 0), sum(p.mat > 0.05))
})

test_that("sig.stars suppresses the pch layer rather than drawing both", {
  p <- ggcorrplot(corr, p.mat = p.mat, sig.stars = TRUE, lab = TRUE)
  expect_equal(geoms(p), c("GeomTile", "GeomText"))
  expect_false("GeomPoint" %in% geoms(p))
})

test_that("lab and insig = 'pch' coexist as three layers", {
  p <- ggcorrplot(corr, p.mat = p.mat, lab = TRUE, insig = "pch")
  expect_equal(geoms(p), c("GeomTile", "GeomText", "GeomPoint"))
})


# ---- triangle and diagonal selection ----------------------------------------

test_that("type and show.diag select exactly the expected number of cells", {
  # off-diagonal cells of one triangle
  tri <- n * (n - 1) / 2
  expect_equal(layer_rows(ggcorrplot(corr, type = "lower")), tri)
  expect_equal(layer_rows(ggcorrplot(corr, type = "upper")), tri)
  expect_equal(layer_rows(ggcorrplot(corr, type = "lower", show.diag = TRUE)), tri + n)
  expect_equal(layer_rows(ggcorrplot(corr, type = "upper", show.diag = TRUE)), tri + n)
  expect_equal(layer_rows(ggcorrplot(corr, type = "full")), n * n)
  expect_equal(layer_rows(ggcorrplot(corr, show.diag = FALSE)), n * n - n)
})

test_that("show.diag defaults to TRUE for type = 'full' and FALSE otherwise", {
  expect_equal(layer_rows(ggcorrplot(corr)),
               layer_rows(ggcorrplot(corr, show.diag = TRUE)))
  expect_equal(layer_rows(ggcorrplot(corr, type = "lower")),
               layer_rows(ggcorrplot(corr, type = "lower", show.diag = FALSE)))
})

test_that("the lower triangle keeps cells below the diagonal, the upper above", {
  lo <- ggcorrplot(corr, type = "lower")$data
  up <- ggcorrplot(corr, type = "upper")$data
  # levels run in matrix row order, so column index < row index is the lower half
  expect_true(all(as.integer(lo$Var1) > as.integer(lo$Var2)))
  expect_true(all(as.integer(up$Var1) < as.integer(up$Var2)))
})


# ---- the melt contract -------------------------------------------------------

test_that("the plot data carries the documented columns", {
  d <- ggcorrplot(corr)$data
  expect_true(all(c("Var1", "Var2", "value", "pvalue", "signif", "abs_corr") %in% names(d)))
})

test_that("Var1/Var2 are factors whose levels follow matrix row order, not alphabetical", {
  d <- ggcorrplot(corr)$data
  expect_s3_class(d$Var1, "factor")
  expect_identical(levels(d$Var1), colnames(corr))
  expect_identical(levels(d$Var2), colnames(corr))
  # mtcars is not alphabetically ordered -- guards against a silent sort
  expect_false(identical(levels(d$Var1), sort(colnames(corr))))
})

test_that("as.is = TRUE keeps the axis variables in matrix order (as a factor)", {
  # as.is passes through to reshape2::melt, but the axis is then coerced to a
  # factor in matrix (row/column) order so the display order does not depend on
  # how melt happened to render the names (#37)
  d <- ggcorrplot(corr, as.is = TRUE)$data
  expect_s3_class(d$Var1, "factor")
  expect_identical(levels(d$Var1), colnames(corr))
})

test_that("abs_corr is 10x the absolute correlation (drives circle size)", {
  d <- ggcorrplot(corr)$data
  expect_equal(d$abs_corr, abs(d$value) * 10)
})

test_that("p-values are joined by cell name, not by row position", {
  # shuffling p.mat's rows/cols must not change which p-value lands on which cell
  shuffled <- p.mat[rev(seq_len(n)), rev(seq_len(n))]
  a <- ggcorrplot(corr, p.mat = p.mat)$data
  b <- ggcorrplot(corr, p.mat = shuffled)$data
  key <- function(d) paste(d$Var1, d$Var2)
  expect_equal(a$pvalue[order(key(a))], b$pvalue[order(key(b))])
})


# ---- scales ------------------------------------------------------------------

test_that("there is exactly one fill scale, spanning [-1, 1] by default", {
  s <- fill_scale(ggcorrplot(corr))
  expect_equal(s$limits, c(-1, 1))
})

test_that("the default 3-colour palette anchors low/mid/high at -1/0/+1", {
  # class() cannot distinguish gradient2 from gradientn -- both are
  # "ScaleContinuous". Sampling the scale's own mapping can.
  s <- fill_scale(ggcorrplot(corr))
  expect_equal(s$map(0), "#FFFFFF")   # mid = "white"
  expect_equal(s$map(-1), "#0000FF")  # low = "blue"
  expect_equal(s$map(1), "#FF0000")   # high = "red"
})

test_that("the 3-colour path is genuinely DIVERGING: the mid colour stays pinned at zero", {
  # Careful: over the symmetric default limits c(-1, 1), scale_fill_gradient2()
  # and scale_fill_gradientn() produce IDENTICAL colours at every point, so
  # anchor checks alone cannot tell them apart. Only an asymmetric limit
  # separates them -- gradient2 holds the mid colour at midpoint = 0, while
  # gradientn spreads the palette evenly and puts it at the middle of the range.
  # This is the property that makes the scale correct for sign-at-zero data.
  s <- fill_scale(ggcorrplot(corr, legend.limit = c(-1, 3)))
  expect_equal(s$map(0), "#FFFFFF")            # zero is still white
  expect_false(identical(s$map(1), "#FFFFFF")) # the range midpoint is not
})

test_that("a palette whose length is not 3 spreads colours evenly instead of diverging", {
  s <- fill_scale(ggcorrplot(corr, colors = c("blue", "red")))
  expect_equal(s$map(-1), "#0000FF")
  expect_equal(s$map(1), "#FF0000")
  # the midpoint is NOT forced to a mid colour on this path
  expect_false(identical(s$map(0), "#FFFFFF"))
})

test_that("an 11-colour palette maps zero to its own middle colour, not white", {
  pal <- c("#053061", "#2166AC", "#4393C3", "#92C5DE", "#D1E5F0", "#F7F7F7",
           "#FDDBC7", "#F4A582", "#D6604D", "#B2182B", "#67001F")
  s <- fill_scale(ggcorrplot(corr, colors = pal))
  expect_equal(toupper(s$map(-1)), pal[1])
  expect_equal(toupper(s$map(1)), pal[length(pal)])
})

test_that("legend.limit = NULL lets the scale free itself from [-1, 1]", {
  s <- fill_scale(ggcorrplot(cov(mtcars[, 1:4]), legend.limit = NULL))
  expect_null(s$limits)
})

test_that("method = 'circle' maps sizes into the documented [4, 10] range", {
  # assert on the BUILT sizes rather than on scale internals: the range lives in
  # a closure whose layout is a ggplot2 implementation detail
  p <- ggcorrplot(corr, method = "circle")
  sizes <- ggplot2::ggplot_build(p)$data[[1]]$size
  expect_equal(range(sizes), c(4, 10))
  expect_true(any(vapply(p$scales$scales, function(x) "size" %in% x$aesthetics, logical(1))))
})


# ---- coordinate system -------------------------------------------------------

test_that("coord.fixed toggles the aspect ratio, not the coord class", {
  # in ggplot2 >= 4, coord_fixed() and coord_cartesian() share a class; only
  # $ratio distinguishes them. Assert on $ratio.
  expect_equal(ggcorrplot(corr)$coordinates$ratio, 1)
  expect_null(ggcorrplot(corr, coord.fixed = FALSE)$coordinates$ratio)
})


# ---- ordering ----------------------------------------------------------------

test_that("hc.order reorders the axis away from input order", {
  p <- ggcorrplot(corr, hc.order = TRUE)
  expect_false(identical(levels(p$data$Var1), colnames(corr)))
  expect_setequal(levels(p$data$Var1), colnames(corr))
})

test_that("hc.order clusters on the unrounded matrix (#14)", {
  # rounding before clustering manufactures ties that move the dendrogram. This
  # is invisible on a pre-rounded input, so cluster an UNROUNDED matrix here.
  raw <- cor(mtcars)
  ord <- function(m) hclust(as.dist((1 - m) / 2), method = "complete")$order
  expect_false(identical(ord(raw), ord(round(raw, 1))))
  p <- ggcorrplot(raw, hc.order = TRUE, digits = 1)
  expect_identical(levels(p$data$Var1), rownames(raw)[ord(raw)])
})

test_that("hc.order reorders p.mat in lockstep with corr", {
  p <- ggcorrplot(corr, hc.order = TRUE, p.mat = p.mat)
  d <- p$data
  # every cell's pvalue must still be the p-value of its own (Var1, Var2) pair
  expected <- p.mat[cbind(as.character(d$Var1), as.character(d$Var2))]
  expect_equal(d$pvalue, unname(expected))
})

test_that("sig.level is inclusive: p exactly equal to sig.level counts as significant", {
  # `p <= sig.level`, not `p < sig.level`. The two differ only at the boundary,
  # which real data almost never hits -- so only a constructed p-matrix pins it.
  cm <- matrix(c(1, 0.3, 0.3, 1), 2, dimnames = list(c("a", "b"), c("a", "b")))
  pm <- matrix(c(0, 0.05, 0.05, 0), 2, dimnames = list(c("a", "b"), c("a", "b")))

  d <- ggcorrplot(cm, p.mat = pm)$data
  off <- d[d$Var1 != d$Var2, ]
  expect_true(all(off$signif == 1))

  # and, being significant, such a cell must survive insig = "blank"
  db <- ggcorrplot(cm, p.mat = pm, insig = "blank")$data
  offb <- db[db$Var1 != db$Var2, ]
  expect_true(all(offb$value == 0.3))
})

test_that("significance is tested on unrounded p-values", {
  # a p-value of 0.054 must not round to 0.05 and read as significant
  cm <- matrix(c(1, 0.3, 0.3, 1), 2, dimnames = list(c("a", "b"), c("a", "b")))
  pm <- matrix(c(0, 0.054, 0.054, 0), 2, dimnames = list(c("a", "b"), c("a", "b")))
  d <- ggcorrplot(cm, p.mat = pm)$data
  off <- d[d$Var1 != d$Var2, ]
  expect_true(all(off$signif == 0))
})


# ---- draw-time -----------------------------------------------------------------

test_that("every documented configuration survives a real render, not just a build", {
  # ggplot_build() sails past failures that only surface when grobs are made
  configs <- list(
    ggcorrplot(corr),
    ggcorrplot(corr, method = "circle"),
    ggcorrplot(corr, type = "lower", lab = TRUE),
    ggcorrplot(corr, type = "upper", show.diag = TRUE),
    ggcorrplot(corr, hc.order = TRUE, p.mat = p.mat, insig = "blank"),
    ggcorrplot(corr, p.mat = p.mat, sig.stars = TRUE, lab = TRUE),
    ggcorrplot(corr, colors = c("blue", "red")),
    ggcorrplot(corr, coord.fixed = FALSE, title = "t", show.legend = FALSE)
  )
  for (p in configs) expect_s3_class(ggplot2::ggplotGrob(p), "gtable")
})
