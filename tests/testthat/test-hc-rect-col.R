# hc.rect.col: control the outline color of the hc.rect cluster rectangles.
# Default "gray30" keeps existing output byte-identical.

corr <- round(cor(mtcars), 1)
rect_col <- function(p) {
  i <- which(vapply(p$layers, function(l) class(l$geom)[1], "") == "GeomRect")
  p$layers[[i]]$aes_params$colour
}

test_that("hc.rect.col defaults to gray30 (unchanged output)", {
  p <- ggcorrplot(corr, hc.order = TRUE, hc.rect = 3)
  expect_identical(rect_col(p), "gray30")
  # explicit default equals the implicit default (byte-identical layer data)
  expect_equal(
    ggplot2::ggplot_build(ggcorrplot(corr, hc.order = TRUE, hc.rect = 3))$data,
    ggplot2::ggplot_build(ggcorrplot(corr, hc.order = TRUE, hc.rect = 3, hc.rect.col = "gray30"))$data
  )
})

test_that("hc.rect.col sets the rectangle outline color", {
  expect_identical(rect_col(ggcorrplot(corr, hc.order = TRUE, hc.rect = 3, hc.rect.col = "white")), "white")
  expect_identical(rect_col(ggcorrplot(corr, hc.order = TRUE, hc.rect = 3, hc.rect.col = "black")), "black")
  expect_identical(rect_col(ggcorrplot(corr, hc.order = TRUE, hc.rect = 2, hc.rect.col = "#E46726")), "#E46726")
})

test_that("hc.rect.col is inert when no rectangles are drawn", {
  # no hc.rect -> no GeomRect layer, and setting hc.rect.col changes nothing
  p <- ggcorrplot(corr, hc.order = TRUE, hc.rect.col = "white")
  expect_false("GeomRect" %in% vapply(p$layers, function(l) class(l$geom)[1], ""))
  expect_equal(
    ggplot2::ggplot_build(ggcorrplot(corr, hc.order = TRUE))$data,
    ggplot2::ggplot_build(ggcorrplot(corr, hc.order = TRUE, hc.rect.col = "white"))$data
  )
})
