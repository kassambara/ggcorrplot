test_that("coord.fixed toggles the coordinate system", {
  corr <- round(cor(mtcars), 1)
  # default: fixed aspect ratio (ratio == 1)
  expect_equal(ggcorrplot(corr)$coordinates$ratio, 1)
  # opt-out: cartesian coord, no fixed ratio
  expect_null(ggcorrplot(corr, coord.fixed = FALSE)$coordinates$ratio)
})

test_that("coord.fixed default output matches an unconditional coord_fixed()", {
  corr <- round(cor(mtcars), 1)
  d <- ggcorrplot(corr)$coordinates
  expect_identical(class(d), class(ggplot2::coord_fixed()))
  expect_identical(d$ratio, ggplot2::coord_fixed()$ratio)
})

test_that("lab_fontface is applied to the coefficient labels", {
  corr <- round(cor(mtcars), 1)
  g <- ggcorrplot(corr, lab = TRUE, lab_fontface = "bold")
  txt <- g$layers[[which(vapply(
    g$layers, function(L) inherits(L$geom, "GeomText"), logical(1)
  ))]]
  expect_identical(txt$aes_params$fontface, "bold")
  # default stays "plain"
  g0 <- ggcorrplot(corr, lab = TRUE)
  txt0 <- g0$layers[[which(vapply(
    g0$layers, function(L) inherits(L$geom, "GeomText"), logical(1)
  ))]]
  expect_identical(txt0$aes_params$fontface, "plain")
})

test_that("tl.vjust / tl.hjust set the x-axis text justification", {
  corr <- round(cor(mtcars), 1)
  th <- ggplot2::calc_element(
    "axis.text.x", ggcorrplot(corr, tl.vjust = 0.5, tl.hjust = 0)$theme
  )
  expect_equal(th$vjust, 0.5)
  expect_equal(th$hjust, 0)
  # defaults are 1 / 1
  th0 <- ggplot2::calc_element("axis.text.x", ggcorrplot(corr)$theme)
  expect_equal(th0$vjust, 1)
  expect_equal(th0$hjust, 1)
})
