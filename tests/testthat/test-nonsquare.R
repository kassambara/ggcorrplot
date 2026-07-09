# Non-square correlation matrix handling (#5, #10)

test_that("non-square matrix plots in full but errors for clustered/triangular layouts", {
  rect <- cor(mtcars[, 1:3], mtcars[, 4:7])   # 3 x 4

  # type = "full" (the default) works for a non-square matrix
  expect_no_error(ggplot2::ggplot_build(ggcorrplot(rect)))

  # hc.order and the triangular layouts require a square matrix -> clear error
  expect_error(ggcorrplot(rect, hc.order = TRUE), "square")
  expect_error(ggcorrplot(rect, type = "lower"), "square")
  expect_error(ggcorrplot(rect, type = "upper"), "square")
})

test_that("square matrices are unaffected by the non-square guard", {
  sq <- round(cor(mtcars), 1)
  expect_no_error(ggplot2::ggplot_build(ggcorrplot(sq, hc.order = TRUE, type = "lower")))
})

# show.diag = FALSE on a non-square matrix must remove genuine self-pairs by NAME,
# not the positional diagonal (which is meaningless for m x n). Previously
# .remove_diag() ran diag(cormat) <- NA, blanking min(m, n) arbitrary cells.
cells <- function(p) ggplot2::ggplot_build(p)$data[[1]]
pairs <- function(p) {
  d <- ggplot2::ggplot_build(p)$plot$data
  paste(d$Var1, d$Var2)
}

test_that("non-square + show.diag = FALSE with disjoint variables removes no cell", {
  rect <- cor(mtcars[, 1:3], mtcars[, 4:7]) # 3 x 4, rows {mpg,cyl,disp} cols {hp,drat,wt,qsec}
  # no row variable is also a column variable -> there are no self-pairs to drop
  expect_equal(nrow(cells(ggcorrplot(rect, show.diag = FALSE))), length(rect)) # all 12 kept
  # every cell present, in particular the positional (1,1) cell mpg~hp
  expect_true("mpg hp" %in% pairs(ggcorrplot(rect, show.diag = FALSE)))
})

test_that("non-square + show.diag = FALSE blanks self-pairs by name, off the positional diagonal", {
  m <- round(cor(mtcars), 1)
  # rows are the LAST three of the five columns, so the self-pairs disp~disp,
  # hp~hp, wt~wt sit at columns 3, 4, 5 -- NOT on the positional diagonal
  sub <- m[c("disp", "hp", "wt"), c("mpg", "cyl", "disp", "hp", "wt")] # 3 x 5
  p <- ggcorrplot(sub, show.diag = FALSE)
  expect_equal(nrow(cells(p)), length(sub) - 3L) # exactly the 3 self-pairs removed
  pr <- pairs(p)
  expect_false(any(c("disp disp", "hp hp", "wt wt") %in% pr)) # self-pairs gone
  expect_true("disp mpg" %in% pr) # the positional (1,1) cell (a cross-pair) survives
})

test_that("non-square + show.diag = TRUE (the default for full) keeps every cell", {
  rect <- cor(mtcars[, 1:3], mtcars[, 4:7])
  expect_equal(nrow(cells(ggcorrplot(rect))), length(rect)) # default show.diag path unchanged
})
