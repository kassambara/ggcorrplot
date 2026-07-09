# palette = : named colorblind-safe diverging palettes as a convenience shortcut
# for colors. Default NULL leaves colors (and every existing call) unchanged.

corr <- round(cor(mtcars), 1)
fill <- function(p) ggplot2::ggplot_build(p)$data[[1]]

test_that(".ggcorrplot_palette returns the pinned colorblind-safe hex (visual contract)", {
  rdbu <- .ggcorrplot_palette("RdBu")
  puor <- .ggcorrplot_palette("PuOr")
  expect_length(rdbu, 11)
  expect_length(puor, 11)
  # low = cool, center = white, high = warm (matches the default blue/white/red polarity)
  expect_identical(rdbu[c(1, 6, 11)], c("#053061", "#F7F7F7", "#67001F"))
  expect_identical(puor[c(1, 6, 11)], c("#2D004B", "#F7F7F7", "#7F3B08"))
})

test_that("palette = 'RdBu' is exactly equivalent to passing its hex vector to colors", {
  expect_equal(
    fill(ggcorrplot(corr, palette = "RdBu")),
    fill(ggcorrplot(corr, colors = .ggcorrplot_palette("RdBu")))
  )
})

test_that("palette = NULL (the default) leaves the fill unchanged, and RdBu differs from it", {
  expect_equal(fill(ggcorrplot(corr)), fill(ggcorrplot(corr, palette = NULL)))
  # the RdBu ramp is genuinely different from the default blue/white/red
  expect_false(isTRUE(all.equal(
    fill(ggcorrplot(corr))$fill,
    fill(ggcorrplot(corr, palette = "RdBu"))$fill
  )))
})

test_that("palette takes precedence over colors when both are supplied", {
  expect_equal(
    fill(ggcorrplot(corr, palette = "RdBu", colors = c("blue", "white", "red"))),
    fill(ggcorrplot(corr, palette = "RdBu"))
  )
})

test_that("an unknown palette errors", {
  expect_error(ggcorrplot(corr, palette = "viridis"))
  expect_error(ggcorrplot(corr, palette = "rdbu")) # match.arg is case-sensitive
})

test_that("palette also colors a mixed 'number' triangle (same colors source)", {
  # the mixed number colour scale reads the same resolved colors
  p <- ggcorrplot(corr, lower.method = "number", upper.method = "circle", palette = "RdBu")
  expect_silent(ggplot2::ggplot_build(p))
})
