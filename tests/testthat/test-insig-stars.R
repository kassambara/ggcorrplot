# insig = "stars" (P1.1): a standalone significance map -- the SIGNIFICANT cells
# are marked with */**/*** stars, independent of lab. The default insig = "pch"
# is unchanged (covered by test-pmat.R / test-structure.R and the byte-identity
# harness); here we lock the new value.

corr <- round(cor(mtcars), 1)
p.mat <- cor_pmat(mtcars)
n <- ncol(mtcars)

geoms <- function(p) unname(vapply(p$layers, function(l) class(l$geom)[1], character(1)))
star_layer_labels <- function(p) {
  i <- which(geoms(p) == "GeomText")
  ggplot2::ggplot_build(p)$data[[i]]$label
}

test_that("insig = 'stars' adds a text layer of significance stars over the tiles", {
  p <- ggcorrplot(corr, p.mat = p.mat, insig = "stars")
  expect_equal(geoms(p), c("GeomTile", "GeomText"))
})

test_that("stars mark significant cells and leave non-significant cells blank", {
  p <- ggcorrplot(corr, p.mat = p.mat, insig = "stars")
  lbl <- star_layer_labels(p)
  d <- ggplot2::ggplot_build(p)$data[[1]] # tiles carry the p-values? no -- rebuild from p.mat
  # every star string is one of the four valid values
  expect_true(all(lbl %in% c("***", "**", "*", "")))
  # a cell is starred iff its p-value is significant at the corresponding level
  pv <- p.mat[cbind(
    as.character(ggplot2::ggplot_build(p)$plot$data$Var1),
    as.character(ggplot2::ggplot_build(p)$plot$data$Var2)
  )]
  expected <- .sig_stars(unname(pv))
  expect_identical(lbl, expected)
  expect_true(any(lbl == "")) # some non-significant cells exist in mtcars
})

test_that(".sig_stars maps p-values to the documented thresholds", {
  expect_identical(
    .sig_stars(c(0.0005, 0.005, 0.03, 0.2, NA)),
    c("***", "**", "*", "", "")
  )
  # boundaries: cut() is right-closed, so a value exactly on a break falls in the
  # lower (more-significant) bin -- 0.001 -> ***, 0.01 -> **, 0.05 -> *
  expect_identical(.sig_stars(c(0.001, 0.01, 0.05)), c("***", "**", "*"))
  # and just above each break drops a level
  expect_identical(.sig_stars(c(0.0011, 0.011, 0.051)), c("**", "*", ""))
})

test_that("insig = 'stars' needs no lab and does not draw the pch layer", {
  p <- ggcorrplot(corr, p.mat = p.mat, insig = "stars") # lab defaults FALSE
  expect_false("GeomPoint" %in% geoms(p)) # no pch crosses
  # the coefficient labels are NOT shown (lab = FALSE): only the stars text layer
  expect_equal(sum(geoms(p) == "GeomText"), 1)
})

test_that("the default insig ('pch') is unchanged when 'stars' is added to the set", {
  expect_equal(geoms(ggcorrplot(corr, p.mat = p.mat)), c("GeomTile", "GeomPoint"))
  expect_error(ggcorrplot(corr, p.mat = p.mat, insig = "nope"))
})

test_that("the old default vector insig = c('pch', 'blank') still resolves to 'pch'", {
  # adding "stars" to the choice set must not break a caller that passes the
  # previously-documented default vector explicitly: match.arg() only accepts a
  # multi-value arg identical to the full choices, so c("pch", "blank") would
  # error unless we resolve on insig[1]. It must behave exactly like insig = "pch".
  expect_error(ggcorrplot(corr, p.mat = p.mat, insig = c("pch", "blank")), NA)
  a <- ggplot2::ggplot_build(ggcorrplot(corr, p.mat = p.mat, insig = c("pch", "blank")))
  b <- ggplot2::ggplot_build(ggcorrplot(corr, p.mat = p.mat, insig = "pch"))
  expect_equal(a$data, b$data)
})

test_that("insig = 'stars' with sig.stars = TRUE and lab = FALSE still draws the stars", {
  # sig.stars appends stars to the coefficient labels, but those only exist when
  # lab = TRUE; with lab = FALSE the standalone stars must still render so the
  # call is not a bare heatmap.
  p <- ggcorrplot(corr, p.mat = p.mat, insig = "stars", sig.stars = TRUE)
  expect_true("GeomText" %in% geoms(p))
  expect_identical(star_layer_labels(p), .sig_stars(unname(
    p.mat[cbind(
      as.character(ggplot2::ggplot_build(p)$plot$data$Var1),
      as.character(ggplot2::ggplot_build(p)$plot$data$Var2)
    )]
  )))
  # and with lab = TRUE the suffix path draws them, so the standalone layer is
  # suppressed (no double stars): exactly one GeomText, from the labels
  p2 <- ggcorrplot(corr, p.mat = p.mat, insig = "stars", sig.stars = TRUE, lab = TRUE)
  expect_equal(sum(geoms(p2) == "GeomText"), 1)
})

test_that("insig = 'stars' with lab = TRUE appends stars to the labels, no second layer", {
  # lab = TRUE puts the coefficient at the cell center; the stars must be appended
  # to that label (one GeomText: numbers + stars) rather than drawn as a second
  # geom_text overprinting into illegible output.
  p <- ggcorrplot(corr, p.mat = p.mat, insig = "stars", lab = TRUE)
  expect_equal(sum(geoms(p) == "GeomText"), 1L)
  labs <- ggplot2::ggplot_build(p)$data[[which(geoms(p) == "GeomText")]]$label
  # the label carries the number and, on significant cells, the appended stars
  expect_true(any(grepl("[0-9].*\\*", labs)))
  # it equals the sig.stars = TRUE, lab = TRUE label (both are numbers + stars)
  q <- ggcorrplot(corr, p.mat = p.mat, sig.stars = TRUE, lab = TRUE)
  expect_identical(
    ggplot2::ggplot_build(p)$data[[which(geoms(p) == "GeomText")]]$label,
    ggplot2::ggplot_build(q)$data[[which(geoms(q) == "GeomText")]]$label
  )
})

test_that("sig.stars and insig = 'stars' share one star definition", {
  # sig.stars appends the SAME stars to the coefficient labels
  p <- ggcorrplot(corr, p.mat = p.mat, lab = TRUE, sig.stars = TRUE)
  labs <- ggplot2::ggplot_build(p)$data[[2]]$label
  expect_true(any(grepl("\\*", labs)))
})
