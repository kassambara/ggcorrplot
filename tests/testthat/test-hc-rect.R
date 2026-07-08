# Cluster rectangles (P0.3): hc.rect = k draws k boxes around the hierarchical
# clusters, on top of the glyph layer. Requires hc.order = TRUE.

corr <- round(cor(mtcars), 1)
n <- ncol(mtcars)

geoms <- function(p) unname(vapply(p$layers, function(l) class(l$geom)[1], character(1)))
rect_data <- function(p) {
  b <- ggplot2::ggplot_build(p)
  i <- which(geoms(p) == "GeomRect")
  b$data[[i]]
}

test_that("hc.rect = NULL (default) adds no rectangle layer", {
  expect_false("GeomRect" %in% geoms(ggcorrplot(corr, hc.order = TRUE)))
  expect_false("GeomRect" %in% geoms(ggcorrplot(corr)))
})

test_that("hc.rect = k draws exactly k rectangles on top of the tiles", {
  p <- ggcorrplot(corr, hc.order = TRUE, hc.rect = 3)
  expect_equal(geoms(p), c("GeomTile", "GeomRect"))
  expect_equal(nrow(rect_data(p)), 3)
})

test_that("the rectangles are square diagonal blocks at half-integer bounds", {
  rd <- rect_data(ggcorrplot(corr, hc.order = TRUE, hc.rect = 4))
  # square: x-span equals y-span for every block
  expect_equal(rd$xmin, rd$ymin)
  expect_equal(rd$xmax, rd$ymax)
  # half-integer cell boundaries within the n x n grid
  expect_true(all((rd$xmin %% 1) == 0.5))
  expect_true(all((rd$xmax %% 1) == 0.5))
  expect_gte(min(rd$xmin), 0.5)
  expect_lte(max(rd$xmax), n + 0.5)
})

test_that("the k blocks are contiguous and partition all n variables", {
  # this is the correctness property: cutree clusters are contiguous in
  # dendrogram order, so the blocks tile 1..n with no gap or overlap
  for (k in c(1, 2, 3, 5, n)) {
    rd <- rect_data(ggcorrplot(corr, hc.order = TRUE, hc.rect = k))
    lo <- sort(rd$xmin + 0.5)
    hi <- sort(rd$xmax - 0.5)
    expect_equal(nrow(rd), k)
    expect_equal(min(lo), 1) # first block starts at position 1
    expect_equal(max(hi), n) # last block ends at position n
    # each block's end is the next block's start - 1 (no gap, no overlap)
    expect_equal(hi[-k], lo[-1] - 1)
  }
})

test_that("hc.rect requires hc.order = TRUE", {
  expect_error(ggcorrplot(corr, hc.rect = 3), "hc.order")
})

test_that("hc.rect requires type = 'full' (no boxes over a blanked triangle)", {
  expect_error(ggcorrplot(corr, hc.order = TRUE, type = "lower", hc.rect = 3), "full")
  expect_error(ggcorrplot(corr, hc.order = TRUE, type = "upper", hc.rect = 3), "full")
})

test_that("hc.rect refuses to draw a wrong box when the axis is reordered by name", {
  # numeric-looking dimnames make melt() type-convert the axis to a continuous
  # scale sorted by value, silently discarding hc.order -- the box would frame
  # the wrong cells, so it must error rather than mislead (#37 class)
  mn <- corr
  dimnames(mn) <- list(as.character(seq_len(n)), as.character(seq_len(n)))
  expect_error(ggcorrplot(mn, hc.order = TRUE, hc.rect = 3), "numeric-looking")
  expect_error(ggcorrplot(corr, hc.order = TRUE, hc.rect = 3, as.is = TRUE), "as.is")
  # a mixed layout coerces the axis to a factor in cluster order, so there the
  # boxes DO line up even with numeric names
  expect_s3_class(
    ggplot2::ggplotGrob(ggcorrplot(mn,
      hc.order = TRUE, hc.rect = 3, lower.method = "number", upper.method = "circle"
    )),
    "gtable"
  )
})

test_that("hc.rect validates k", {
  expect_error(ggcorrplot(corr, hc.order = TRUE, hc.rect = 0), "between 1")
  expect_error(ggcorrplot(corr, hc.order = TRUE, hc.rect = n + 1), "between 1")
  expect_error(ggcorrplot(corr, hc.order = TRUE, hc.rect = 2.5), "integer")
  expect_error(ggcorrplot(corr, hc.order = TRUE, hc.rect = c(2, 3)), "single")
})

test_that("hc.rect composes with a mixed layout and renders", {
  expect_s3_class(
    ggplot2::ggplotGrob(ggcorrplot(corr,
      hc.order = TRUE, hc.rect = 3,
      lower.method = "number", upper.method = "circle"
    )),
    "gtable"
  )
})

test_that("adding hc.rect does not break partial matching of hc.order/hc.method", {
  # hc.rect shares the already-ambiguous 'hc' prefix; the unique 'hc.o'/'hc.m'
  # abbreviations must still resolve
  expect_true("GeomRect" %in% geoms(ggcorrplot(corr, hc.o = TRUE, hc.rect = 2)))
  expect_silent(ggcorrplot(corr, hc.order = TRUE, hc.m = "average"))
})
