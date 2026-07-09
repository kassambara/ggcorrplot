# scale.square: size-scaled squares (corrplot-style) for method = "square".
# Default FALSE keeps method = "square" a constant full-cell geom_tile.

corr <- round(cor(mtcars), 1)
geom1 <- function(p) class(p$layers[[1]]$geom)[1]
shape1 <- function(p) p$layers[[1]]$aes_params$shape

test_that("scale.square defaults to FALSE: method = 'square' stays a full tile", {
  p <- ggcorrplot(corr)
  expect_identical(geom1(p), "GeomTile")
  # explicit FALSE == implicit default (byte-identical built data)
  expect_equal(
    ggplot2::ggplot_build(ggcorrplot(corr))$data,
    ggplot2::ggplot_build(ggcorrplot(corr, scale.square = FALSE))$data
  )
})

test_that("scale.square = TRUE draws size-scaled squares (shape 22, sized by |r|)", {
  p <- ggcorrplot(corr, scale.square = TRUE)
  expect_identical(geom1(p), "GeomPoint")
  expect_identical(shape1(p), 22)
  # the point size maps to abs_corr (magnitude), and there is a size scale
  b <- ggplot2::ggplot_build(p)
  expect_true("size" %in% names(b$data[[1]]))
  # stronger correlations -> larger size than weak ones
  pd <- b$plot$data
  szmap <- b$data[[1]]$size
  expect_gt(max(szmap), min(szmap)) # sizes actually vary
})

test_that("scale.square has no effect for method = 'circle'", {
  a <- ggcorrplot(corr, method = "circle")
  b <- ggcorrplot(corr, method = "circle", scale.square = TRUE)
  expect_equal(shape1(a), 21)
  expect_equal(shape1(b), 21) # still circles, unchanged
  expect_equal(
    ggplot2::ggplot_build(a)$data,
    ggplot2::ggplot_build(b)$data
  )
})

test_that("scale.square sizes the square region in a mixed layout", {
  # a mixed layout with a 'square' triangle -> that triangle becomes sized squares
  p <- ggcorrplot(corr, lower.method = "square", upper.method = "number", scale.square = TRUE)
  geoms <- vapply(p$layers, function(l) class(l$geom)[1], "")
  expect_true("GeomPoint" %in% geoms) # the square triangle is now a sized point layer
  sq <- p$layers[[which(geoms == "GeomPoint")[1]]]
  expect_identical(sq$aes_params$shape, 22)
})
