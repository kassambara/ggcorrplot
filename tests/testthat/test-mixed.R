# Mixed layout (P0.1, issue #85): a different glyph per triangle, requested via
# lower.method / upper.method; the diagonal always shows the variable names. The
# single-method path must stay byte-identical (that is covered by test-structure.R
# and the byte-identity harness); here we lock the structure of the mixed plot.

corr <- round(cor(mtcars), 1)
p.mat <- cor_pmat(mtcars)
n <- ncol(mtcars)
tri <- n * (n - 1) / 2

geoms <- function(p) unname(vapply(p$layers, function(l) class(l$geom)[1], character(1)))
has_scale <- function(p, aes) {
  any(vapply(p$scales$scales, function(s) aes %in% s$aesthetics, logical(1)))
}
axis_labels <- function(p, which) {
  b <- ggplot2::ggplot_build(p)
  b$layout$panel_params[[1]][[which]]$get_labels()
}

test_that("no method.* argument leaves the single-method path untouched", {
  # mixed mode must NOT fire when the per-region args are absent
  base <- ggcorrplot(corr)
  expect_equal(geoms(base), "GeomTile")
  expect_false(has_scale(base, "colour"))
})

test_that("lower.method + upper.method draws a glyph per triangle plus a name diagonal", {
  p <- ggcorrplot(corr, lower.method = "number", upper.method = "circle")
  # lower numbers (text), upper circles (points), diagonal names (text)
  expect_equal(geoms(p), c("GeomText", "GeomPoint", "GeomText"))
  # each region holds the right number of cells
  rows <- vapply(ggplot2::ggplot_build(p)$data, nrow, integer(1))
  expect_equal(sort(rows), sort(c(tri, tri, n))) # two triangles + diagonal
})

test_that("the two axes share one variable order so the diagonal is a straight line", {
  p <- ggcorrplot(corr, lower.method = "number", upper.method = "circle")
  expect_identical(axis_labels(p, "x"), axis_labels(p, "y"))
  expect_identical(axis_labels(p, "x"), colnames(corr))
})

test_that("a 'number' region adds a colour scale with its guide suppressed", {
  p <- ggcorrplot(corr, lower.method = "number", upper.method = "circle")
  expect_true(has_scale(p, "colour"))
  # the fill scale (from the circle region) is still present for the legend
  expect_true(has_scale(p, "fill"))
})

test_that("a numbers-only mixed plot keeps a legend for its value colours", {
  colour_scale <- function(p) {
    s <- p$scales$scales
    s[[which(vapply(s, function(x) "colour" %in% x$aesthetics, logical(1)))]]
  }
  # both triangles numeric: no fill glyph, so the colour scale must carry the
  # legend (named), otherwise the plot would be colour-encoded but legend-less
  nn <- ggcorrplot(corr, lower.method = "number", upper.method = "number")
  expect_equal(colour_scale(nn)$name, "Corr")
  # a fill glyph present: the fill scale carries the legend, colour is redundant
  nc <- ggcorrplot(corr, lower.method = "number", upper.method = "circle")
  expect_s3_class(colour_scale(nc)$name, "waiver")
})

test_that("no 'number' region means no colour scale", {
  p <- ggcorrplot(corr, lower.method = "square", upper.method = "circle")
  expect_false(has_scale(p, "colour"))
  expect_equal(geoms(p), c("GeomTile", "GeomPoint", "GeomText"))
})

test_that("the diagonal shows the variable names in a mixed layout", {
  p <- ggcorrplot(corr, lower.method = "square", upper.method = "circle")
  # the last layer is the diagonal name text, one per variable
  diag_layer <- p$layers[[length(p$layers)]]
  expect_s3_class(diag_layer$geom, "GeomText")
  d <- diag_layer$data
  expect_equal(nrow(d), n)
  expect_true(all(as.character(d$Var1) == as.character(d$Var2)))
})

test_that("an unset triangle inherits the base method", {
  p <- ggcorrplot(corr, upper.method = "circle") # method defaults to square
  # lower triangle should be squares (tiles), upper circles, diagonal names
  expect_true(all(c("GeomTile", "GeomPoint") %in% geoms(p)))
})

test_that("mixed mode forces the full matrix regardless of type", {
  p <- ggcorrplot(corr, lower.method = "number", upper.method = "circle", type = "lower")
  rows <- sum(vapply(ggplot2::ggplot_build(p)$data, nrow, integer(1)))
  expect_equal(rows, n * n) # full matrix, not just a triangle
})

test_that("mixed labels format like the coefficient labels do", {
  # the number glyph reuses .format_coef, so nsmall / leading.zero apply
  p <- ggcorrplot(corr, lower.method = "number", nsmall = 2L)
  lower <- ggplot2::ggplot_build(p)$data[[1]]
  expect_true(all(grepl("\\.[0-9]{2}$", lower$label)))
})

test_that("a mixed layout requires a square matrix", {
  ns <- cor(mtcars)[1:3, 1:5]
  expect_error(ggcorrplot(ns, lower.method = "number"), "square")
})

test_that("a mixed layout requires matching row and column names (no silent mislabel)", {
  m <- round(cor(mtcars[, 1:4]), 1)
  colnames(m) <- c("w", "x", "y", "z") # square, but names differ from rownames
  expect_error(
    ggcorrplot(m, lower.method = "number", upper.method = "circle"),
    "matching row and column names"
  )
  # a genuine correlation matrix (rownames == colnames) is unaffected
  expect_s3_class(
    ggplot2::ggplotGrob(ggcorrplot(round(cor(mtcars[, 1:4]), 1),
      lower.method = "number", upper.method = "circle"
    )),
    "gtable"
  )
})

test_that("per-triangle methods are validated", {
  expect_error(ggcorrplot(corr, lower.method = "pie"))
  expect_error(ggcorrplot(corr, upper.method = "wedge"))
})

test_that("a number region shows the true coefficient, never a blanked 0, under insig='blank'", {
  # regression: the shared insig='blank' zeroing must not reach the number glyph,
  # or non-significant cells would print a wrong "0" instead of their coefficient
  suppressMessages(
    p <- ggcorrplot(corr,
      lower.method = "number", upper.method = "circle",
      p.mat = p.mat, insig = "blank"
    )
  )
  lower <- ggplot2::ggplot_build(p)$data[[1]]
  expect_false(any(lower$label == "0"))
  # the labels are the real rounded coefficients
  expect_true(all(grepl("^-?[01]?\\.[0-9]+$|^-?1$", lower$label)))
})

test_that("mixed mode messages when it ignores single-method overlay arguments", {
  expect_message(
    ggcorrplot(corr, lower.method = "number", p.mat = p.mat),
    "mixed layout"
  )
  expect_message(
    ggcorrplot(corr, lower.method = "number", lab = TRUE),
    "mixed layout"
  )
  # no message for a clean mixed call that sets no overlay arguments
  expect_no_message(
    ggcorrplot(corr, lower.method = "number", upper.method = "circle")
  )
})

test_that("numeric-looking, non-sorted dimnames do not scramble the mixed grid (#37)", {
  # reshape2::melt type-converts numeric names to their VALUES; the region split
  # and axes must key off grid POSITION, not the value, or the diagonal bends
  m <- round(cor(mtcars[, 1:5]), 1)
  dimnames(m) <- list(c("50", "10", "30", "20", "40"), c("50", "10", "30", "20", "40"))
  p <- ggcorrplot(m, lower.method = "number", upper.method = "circle")
  # the axes keep the matrix order (not sorted numerically)
  expect_identical(axis_labels(p, "x"), c("50", "10", "30", "20", "40"))
  expect_identical(axis_labels(p, "x"), axis_labels(p, "y"))
  # the diagonal "name" cells sit exactly on x == y
  b <- ggplot2::ggplot_build(p)
  diag_d <- p$layers[[3]]$data
  xpos <- match(as.character(diag_d$Var1), axis_labels(p, "x"))
  ypos <- match(as.character(diag_d$Var2), axis_labels(p, "y"))
  expect_equal(xpos, ypos)
})

test_that("as.is = TRUE works in a mixed layout without warning or scrambling", {
  m <- round(cor(mtcars[, 1:5]), 1)
  expect_no_warning(
    p <- ggcorrplot(m, lower.method = "number", upper.method = "circle", as.is = TRUE)
  )
  k <- ncol(m)
  rows <- sort(vapply(ggplot2::ggplot_build(p)$data, nrow, integer(1)))
  expect_equal(rows, sort(c(k, k * (k - 1) / 2, k * (k - 1) / 2)))
})

test_that("the mixed arguments do not break partial matching of existing arguments", {
  # lower.method/upper.method deliberately avoid the prefixes of commonly-used
  # existing arguments, so their abbreviations keep resolving uniquely:
  # `meth`/`m` still reach `method`, and `d`/`di` still reach `digits`.
  expect_s3_class(ggcorrplot(corr, meth = "circle")$layers[[1]]$geom, "GeomPoint")
  expect_s3_class(ggcorrplot(corr, m = "circle")$layers[[1]]$geom, "GeomPoint")
  # `digits` abbreviations must still work (no `diag.*` formal shadows them)
  expect_silent(ggcorrplot(corr, d = 2))
  expect_silent(ggcorrplot(corr, di = 2))
})

test_that("a fully-NA variable is not resurrected as an empty band in a mixed layout", {
  # the mixed layout pins the discrete axes with drop = FALSE, so a variable that
  # is entirely NA (absent after na.rm melt) must be dropped from the axis levels
  # rather than shown as an empty labelled row/column
  m <- round(cor(mtcars[, 1:4]), 1)
  m[2, ] <- NA
  m[, 2] <- NA
  p <- ggcorrplot(m, lower.method = "number", upper.method = "circle")
  labs <- ggplot2::ggplot_build(p)$layout$panel_params[[1]]$x$get_labels()
  present <- colnames(m)[colSums(!is.na(m)) > 0]
  expect_setequal(labs, present)
  expect_false(colnames(m)[2] %in% labs) # the all-NA variable is gone
})

test_that("an unnamed matrix still draws a discrete positional axis in a mixed layout", {
  m <- round(cor(mtcars[, 1:4]), 1)
  dimnames(m) <- NULL
  p <- ggcorrplot(m, lower.method = "number", upper.method = "circle")
  expect_s3_class(p$data$Var1, "factor")
  expect_s3_class(ggplot2::ggplotGrob(p), "gtable")
})

test_that("mixed composes with hc.order without error and stays full", {
  p <- ggcorrplot(corr, lower.method = "number", upper.method = "circle", hc.order = TRUE)
  expect_s3_class(ggplot2::ggplotGrob(p), "gtable")
  # reordered axis is still shared between x and y
  expect_identical(axis_labels(p, "x"), axis_labels(p, "y"))
})

test_that("a ragged NA pattern keeps both axes on one matrix-ordered scale", {
  # melt(na.rm = TRUE) drops NA cells, so the order the variables first APPEAR in
  # the melted frame is not the matrix order once the NA pattern is ragged. Taking
  # the two axes independently from that appearance order put them out of step and
  # landed the diagonal name region on cells that are not self-pairs -- a silent
  # mislabel on a plot that renders cleanly. Both axes must stay in matrix order.
  m <- round(cor(mtcars)[1:4, 1:4], 2)
  m["cyl", "mpg"] <- NA
  m["mpg", "cyl"] <- NA
  p <- ggcorrplot(m, lower.method = "number", upper.method = "circle")
  expect_identical(axis_labels(p, "x"), axis_labels(p, "y"))
  expect_identical(axis_labels(p, "x"), colnames(m))
})

test_that("the mixed diagonal names only ever sit on genuine self-pairs", {
  # the failure this guards is positional, so assert on the drawn cells: every
  # label in the name layer must sit where its own row and column meet.
  expect_diagonal_is_self_paired <- function(m) {
    p <- ggcorrplot(m, lower.method = "number", upper.method = "circle")
    b <- ggplot2::ggplot_build(p)
    name_layer <- b$data[[length(b$data)]] # regions are added lower, upper, diagonal
    xl <- axis_labels(p, "x")
    yl <- axis_labels(p, "y")
    expect_identical(xl[name_layer$x], yl[name_layer$y])
    expect_identical(as.character(name_layer$label), xl[name_layer$x])
  }
  # a pair of variables that never co-occur -> one NA cell per triangle
  d <- mtcars[, 1:4]
  d[1:16, 1] <- NA
  d[17:32, 2] <- NA
  expect_diagonal_is_self_paired(round(suppressWarnings(
    cor(d, use = "pairwise.complete.obs")
  ), 2))
  # a zero-variance column -> cor() returns NA for every pair involving it
  dc <- mtcars[, 1:4]
  dc[, 2] <- 1
  expect_diagonal_is_self_paired(suppressWarnings(round(cor(dc), 2)))
  # an asymmetric NA pattern (row blanked, column kept)
  ma <- round(cor(mtcars[, 1:5]), 2)
  ma[1, ] <- NA
  expect_diagonal_is_self_paired(ma)
})

test_that("an unnamed ragged matrix orders the mixed axes numerically, not by appearance", {
  m <- round(cor(mtcars)[1:4, 1:4], 2)
  m[2, 1] <- NA
  m[1, 2] <- NA
  dimnames(m) <- NULL
  p <- ggcorrplot(m, lower.method = "number", upper.method = "circle")
  expect_identical(axis_labels(p, "x"), axis_labels(p, "y"))
  expect_identical(axis_labels(p, "x"), as.character(1:4))
})

test_that("tl.col and tl.cex reach the mixed diagonal names", {
  # tl.col was once accepted and silently dropped (#44); the mixed name region
  # takes its colour and size from these arguments, so pin them.
  p <- ggcorrplot(corr, lower.method = "number", upper.method = "circle")
  expect_identical(p$layers[[3]]$aes_params$colour, "black")
  expect_equal(p$layers[[3]]$aes_params$size, 12 / 3)
  q <- ggcorrplot(corr,
    lower.method = "number", upper.method = "circle",
    tl.col = "steelblue", tl.cex = 18
  )
  expect_identical(q$layers[[3]]$aes_params$colour, "steelblue")
  expect_equal(q$layers[[3]]$aes_params$size, 18 / 3)
})
