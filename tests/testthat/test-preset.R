# preset = "publication": a one-token bundle of publication-grade defaults (white
# cell outlines + the colorblind-safe RdBu palette). It fills only the arguments
# the caller did NOT supply, so explicit arguments always win, and preset = NULL
# (the default) changes nothing.

corr <- round(cor(mtcars), 1)
fill <- function(p) ggplot2::ggplot_build(p)$data[[1]]$fill
outline <- function(p) {
  i <- which(vapply(p$layers, function(l) class(l$geom)[1], "") %in% c("GeomTile", "GeomPoint"))[1]
  p$layers[[i]]$aes_params$colour
}

test_that("preset = 'publication' sets white outlines and the RdBu palette in one token", {
  p <- ggcorrplot(corr, preset = "publication")
  expect_identical(outline(p), "white")
  expect_equal(fill(p), fill(ggcorrplot(corr, palette = "RdBu", outline.color = "white")))
})

test_that("preset = NULL (the default) leaves every existing call unchanged", {
  expect_equal(
    ggplot2::ggplot_build(ggcorrplot(corr))$data,
    ggplot2::ggplot_build(ggcorrplot(corr, preset = NULL))$data
  )
  # the default outline is still gray, not white
  expect_identical(outline(ggcorrplot(corr)), "gray")
})

test_that("an explicit outline.color overrides the preset", {
  expect_identical(outline(ggcorrplot(corr, preset = "publication", outline.color = "black")), "black")
})

test_that("an explicit colors overrides the preset palette (colors is not clobbered)", {
  # user pins colors -> preset must NOT swap in RdBu
  expect_equal(
    fill(ggcorrplot(corr, preset = "publication", colors = c("blue", "white", "red"))),
    fill(ggcorrplot(corr))
  )
})

test_that("an explicit palette overrides the preset palette", {
  expect_equal(
    fill(ggcorrplot(corr, preset = "publication", palette = "PuOr")),
    fill(ggcorrplot(corr, palette = "PuOr"))
  )
})

test_that("an unknown preset errors", {
  expect_error(ggcorrplot(corr, preset = "fancy"))
  expect_error(ggcorrplot(corr, preset = "Publication")) # match.arg is case-sensitive
})
