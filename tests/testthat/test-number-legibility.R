# The mixed "number" glyph maps the coefficient TEXT onto the fill ramp, which is
# white at zero -- so a near-zero coefficient rendered invisible against the
# panel, and a blank cell in a correlogram reads as removed/non-significant. The
# text ramp is darkened to a legibility floor; the FILL ramp must be untouched.

corr <- round(cor(mtcars[, 1:6]), 2)

relative_luminance <- function(cols) {
  vapply(cols, function(col) {
    chan <- grDevices::col2rgb(col)[, 1] / 255
    chan <- ifelse(chan <= 0.03928, chan / 12.92, ((chan + 0.055) / 1.055)^2.4)
    sum(c(0.2126, 0.7152, 0.0722) * chan)
  }, numeric(1), USE.NAMES = FALSE)
}
contrast_on_white <- function(cols) 1.05 / (relative_luminance(cols) + 0.05)

test_that("the text ramp clears 4.5:1 contrast on white while the input ramp does not", {
  default <- c("blue", "white", "red")
  expect_true(any(contrast_on_white(default) < 4.5)) # white fails, as it must
  expect_true(all(contrast_on_white(ggcorrplot:::.legible_text_colors(default)) >= 4.5))
  for (pal in c("RdBu", "PuOr")) {
    stops <- ggcorrplot:::.ggcorrplot_palette(pal)
    darkened <- ggcorrplot:::.legible_text_colors(stops)
    expect_length(darkened, length(stops))
    expect_true(all(contrast_on_white(darkened) >= 4.5))
  }
})

test_that("a stop already dark enough is returned untouched", {
  # only the pale middle of a diverging ramp moves; the saturated ends keep their
  # exact values, so the warm/cool polarity is unchanged.
  expect_identical(ggcorrplot:::.legible_text_colors("blue"), "blue")
  expect_identical(ggcorrplot:::.legible_text_colors("#053061"), "#053061")
  expect_identical(ggcorrplot:::.legible_text_colors("black"), "black")
  darkened <- ggcorrplot:::.legible_text_colors(c("blue", "white", "red"))
  expect_identical(darkened[1], "blue")
  expect_false(identical(darkened[2], "white"))
})

test_that("darkening preserves the sign of each channel difference (hue is kept)", {
  # scaling the linearised channels by a common factor keeps their ratios, so a
  # reddish stop stays reddish and a bluish stop stays bluish.
  for (col in c("#F4A582", "#92C5DE", "#FDDBC7", "#D1E5F0")) {
    before <- grDevices::col2rgb(col)[, 1]
    after <- grDevices::col2rgb(ggcorrplot:::.legible_text_colors(col))[, 1]
    expect_equal(sign(before["red"] - before["blue"]),
                 sign(after["red"] - after["blue"]))
  }
})

test_that("only the mixed number glyph uses the darkened ramp; fill is untouched", {
  fill_of <- function(p) unique(ggplot2::ggplot_build(p)$data[[1]]$fill)
  # a non-mixed plot gains no colour scale at all
  expect_false(any(vapply(
    ggcorrplot(corr)$scales$scales,
    function(s) "colour" %in% s$aesthetics, logical(1)
  )))
  # the square/circle fills are identical with and without a "number" region
  mixed_fills <- fill_of(ggcorrplot(corr, lower.method = "square", upper.method = "circle"))
  expect_true(all(mixed_fills %in% fill_of(ggcorrplot(corr))))
  # and a mixed plot that draws numbers still carries a colour scale for them
  p <- ggcorrplot(corr, lower.method = "number", upper.method = "circle")
  expect_true(any(vapply(p$scales$scales, function(s) "colour" %in% s$aesthetics, logical(1))))
})

test_that("near-zero coefficients are drawn legibly rather than invisibly", {
  set.seed(42)
  x <- matrix(stats::rnorm(300 * 6), 300, 6,
              dimnames = list(NULL, paste0("item", seq_len(6))))
  weak <- round(stats::cor(x), 2)
  expect_true(max(abs(weak[upper.tri(weak)])) < 0.2) # genuinely weak input
  p <- ggcorrplot(weak, lower.method = "number", upper.method = "number")
  drawn <- ggplot2::ggplot_build(p)$data
  number_cols <- unlist(lapply(drawn[seq_len(2)], function(d) d$colour))
  expect_true(all(contrast_on_white(number_cols) >= 4.5))
})
