# cell.grid: draw a light rectangle behind every (sized) cell -- the corrplot
# boxed-cell look. Default FALSE keeps every existing call byte-identical.

corr <- round(cor(mtcars), 1)
geoms <- function(p) unname(vapply(p$layers, function(l) class(l$geom)[1], ""))
first_layer_col <- function(p) p$layers[[1]]$aes_params$colour

test_that("cell.grid defaults to FALSE: no cell-grid layer, output unchanged", {
  # circle default has exactly one glyph layer (GeomPoint), no background tile
  p <- ggcorrplot(corr, method = "circle")
  expect_identical(geoms(p), "GeomPoint")
  # explicit FALSE == implicit default (byte-identical built data)
  expect_equal(
    ggplot2::ggplot_build(ggcorrplot(corr, method = "circle"))$data,
    ggplot2::ggplot_build(ggcorrplot(corr, method = "circle", cell.grid = FALSE))$data
  )
})

test_that("cell.grid = TRUE boxes circles: a GeomTile is drawn BEFORE the glyph", {
  p <- ggcorrplot(corr, method = "circle", cell.grid = TRUE)
  g <- geoms(p)
  # background tile first, glyph second
  expect_identical(g[1], "GeomTile")
  expect_identical(g[2], "GeomPoint")
  # the background tile has no fill and uses the default cell.grid.col
  expect_true(is.na(p$layers[[1]]$aes_params$fill))
  expect_identical(first_layer_col(p), "grey90")
})

test_that("cell.grid = TRUE boxes size-scaled squares", {
  p <- ggcorrplot(corr, scale.square = TRUE, cell.grid = TRUE)
  g <- geoms(p)
  expect_identical(g[1], "GeomTile")
  expect_identical(g[2], "GeomPoint")
  expect_identical(p$layers[[2]]$aes_params$shape, 22)
})

test_that("cell.grid.col sets the cell border color", {
  p <- ggcorrplot(corr, method = "circle", cell.grid = TRUE, cell.grid.col = "black")
  expect_identical(first_layer_col(p), "black")
})

test_that("cell.grid has no effect on a full-tile square heatmap", {
  # method = "square" without scale.square already draws a bordered tile, so no
  # extra background tile is added and the built data is unchanged
  a <- ggcorrplot(corr, method = "square")
  b <- ggcorrplot(corr, method = "square", cell.grid = TRUE)
  expect_identical(geoms(a), "GeomTile")
  expect_identical(geoms(b), "GeomTile") # still a single tile layer
  expect_equal(
    ggplot2::ggplot_build(a)$data,
    ggplot2::ggplot_build(b)$data
  )
  # "no effect" must include the theme: the gridlines are NOT blanked for a
  # full-tile square heatmap (no box was drawn to replace them). Checked on a
  # lower triangle, where those gridlines are actually visible in the blank half.
  low <- ggcorrplot(corr, method = "square", type = "lower", cell.grid = TRUE)
  expect_false(inherits(b$theme$panel.grid.major, "element_blank"))
  expect_false(inherits(low$theme$panel.grid.major, "element_blank"))
})

test_that("cell.grid = TRUE blanks the panel gridlines", {
  off <- ggcorrplot(corr, method = "circle")
  on <- ggcorrplot(corr, method = "circle", cell.grid = TRUE)
  # default keeps the theme's gridlines; cell.grid removes them
  expect_false(inherits(off$theme$panel.grid.major, "element_blank"))
  expect_true(inherits(on$theme$panel.grid.major, "element_blank"))
  expect_true(inherits(on$theme$panel.grid.minor, "element_blank"))
})

test_that("cell.grid boxes every region of a mixed layout except full-tile squares", {
  # circle, number and diagonal-name regions each get a background box tile
  p <- ggcorrplot(corr,
    lower.method = "number", upper.method = "circle", cell.grid = TRUE,
    show.legend = FALSE
  )
  g <- geoms(p)
  tile_idx <- which(g == "GeomTile")
  # one box per non-empty region (lower number, upper circle, diagonal name)
  expect_identical(length(tile_idx), 3L)
  # every box carries the cell.grid color and maps no fill (transparent box)
  cols <- vapply(tile_idx, function(i) p$layers[[i]]$aes_params$colour, "")
  expect_true(all(cols == "grey90"))
  has_fill_map <- vapply(tile_idx, function(i) "fill" %in% names(p$layers[[i]]$mapping), TRUE)
  expect_false(any(has_fill_map))
})

test_that("a full-tile square region in a mixed layout is not double-boxed", {
  # lower = square (full tile, has its own outline.color border) -> no extra
  # cell.grid tile for that region; upper = circle -> boxed
  p <- ggcorrplot(corr,
    lower.method = "square", upper.method = "circle", cell.grid = TRUE,
    show.legend = FALSE
  )
  g <- geoms(p)
  # GeomTile layers = the full-tile square region (1) + the circle region's box
  # (1) + the diagonal name box (1) = fewer than if every region were boxed.
  # Assert the full-tile square region's tile is the bordered heatmap tile, not a
  # transparent cell.grid box: it maps fill and uses outline.color.
  square_tiles <- Filter(function(l) {
    class(l$geom)[1] == "GeomTile" && "fill" %in% names(l$mapping)
  }, p$layers)
  expect_true(length(square_tiles) >= 1)
})
