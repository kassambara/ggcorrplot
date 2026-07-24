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
  # a stop already at or below the cap is untouched, so a saturated dark end of
  # the ramp keeps its exact value and the warm/cool polarity is unchanged.
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

test_that("a partly transparent stop is left alone rather than made opaque", {
  # col2rgb() drops alpha unless asked for it, so darkening a translucent stop
  # would have re-emitted it opaque -- leaving one text layer with mixed alpha
  # while the fill kept a uniform one. Its luminance over the panel also depends
  # on what it is composited with, so the contrast target does not apply.
  translucent <- c("#0000FF80", "#FFFFFF80", "#FF000080")
  expect_identical(ggcorrplot:::.legible_text_colors(translucent), translucent)
  expect_identical(ggcorrplot:::.legible_text_colors("transparent"), "transparent")
  # an explicitly opaque colour is still darkened
  expect_false(identical(ggcorrplot:::.legible_text_colors("#FFFFFFFF"), "#FFFFFFFF"))
  # and the drawn text keeps one uniform alpha across the whole ramp
  p <- ggcorrplot(round(stats::cor(mtcars[, 1:4]), 2),
    lower.method = "number", upper.method = "circle", colors = translucent
  )
  drawn <- ggplot2::ggplot_build(p)$data[[1]]$colour
  expect_length(unique(substr(drawn, 8, 9)), 1L)
})

test_that("the legibility floor applies to a light panel only", {
  # Darkening assumes dark text reads against the panel. On a dark theme the pale
  # middle of the ramp is already the readable end, so darkening it there would
  # bury the text instead -- the ramp is left as given.
  expect_true(ggcorrplot:::.panel_is_light(ggplot2::theme_minimal))
  expect_true(ggcorrplot:::.panel_is_light(ggplot2::theme_bw))
  expect_true(ggcorrplot:::.panel_is_light(ggplot2::theme_gray))
  expect_false(ggcorrplot:::.panel_is_light(ggplot2::theme_dark))
  # a theme object, not just a theme function
  expect_false(ggcorrplot:::.panel_is_light(ggplot2::theme_dark()))
  # anything unresolvable falls back to "light", i.e. the default behaviour
  expect_true(ggcorrplot:::.panel_is_light(NULL))
  expect_true(ggcorrplot:::.panel_is_light("not a theme"))
})

test_that("a dark theme keeps the ramp as given, so its numbers stay readable", {
  contrast <- function(a, b) {
    la <- relative_luminance(a)
    lb <- relative_luminance(b)
    (max(la, lb) + 0.05) / (min(la, lb) + 0.05)
  }
  set.seed(42)
  x <- matrix(stats::rnorm(300 * 6), 300, 6,
              dimnames = list(NULL, paste0("item", seq_len(6))))
  weak <- round(stats::cor(x), 2)
  drawn <- function(th) {
    p <- ggcorrplot(weak, lower.method = "number", upper.method = "number", ggtheme = th)
    unique(ggplot2::ggplot_build(p)$data[[1]]$colour)
  }
  # theme_dark's panel is grey50: the undarkened pale middle reads against it
  dark_cols <- drawn(ggplot2::theme_dark)
  expect_true(all(vapply(dark_cols, contrast, numeric(1), "grey50") >= 3.5))
  # and the default light panel still gets the darkened ramp
  expect_false(identical(sort(dark_cols), sort(drawn(ggplot2::theme_minimal))))
})

test_that("a dark background set via plot.background is detected too", {
  # theme_minimal (our default) and theme_void blank panel.background, so a dark
  # recipe built on either carries its color on plot.background. Reading only
  # panel.background classified those as light and darkened the text onto a dark
  # panel -- the exact failure the light/dark gate exists to prevent.
  dark_plot_bg <- ggplot2::theme_minimal() +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = "grey10", colour = NA))
  expect_false(ggcorrplot:::.panel_is_light(dark_plot_bg))
  expect_false(ggcorrplot:::.panel_is_light(
    ggplot2::theme_void() + ggplot2::theme(plot.background = ggplot2::element_rect(fill = "black"))
  ))
  # a fill inherited from `rect` must resolve as well
  expect_false(ggcorrplot:::.panel_is_light(
    ggplot2::`%+replace%`(
      ggplot2::theme_gray(),
      ggplot2::theme(
        rect = ggplot2::element_rect(fill = "grey10"),
        panel.background = ggplot2::element_rect()
      )
    )
  ))
  # panel.background still wins when it paints one
  expect_true(ggcorrplot:::.panel_is_light(
    ggplot2::theme_gray() + ggplot2::theme(plot.background = ggplot2::element_rect(fill = "black"))
  ))
})

test_that("a transparent background is not mistaken for a dark one", {
  # theme_void's plot.background is transparent black; read as a color it is the
  # darkest possible background, but nothing is painted -- the device shows through.
  expect_true(ggcorrplot:::.panel_is_light(ggplot2::theme_void))
  expect_true(ggcorrplot:::.panel_is_light(
    ggplot2::theme_minimal() +
      ggplot2::theme(panel.background = ggplot2::element_rect(fill = "#00000000"))
  ))
  # ... but a transparent panel still falls through to an opaque dark plot background
  expect_false(ggcorrplot:::.panel_is_light(
    ggplot2::theme_minimal() + ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = "#00000000"),
      plot.background = ggplot2::element_rect(fill = "grey10")
    )
  ))
})

test_that("the light/dark threshold sits at the break-even luminance", {
  # below the crossover the plain ramp reads better against the panel, above it the
  # darkened one does; the gate must agree with that on both sides.
  expect_false(ggcorrplot:::.panel_is_light(
    ggplot2::theme_minimal() +
      ggplot2::theme(panel.background = ggplot2::element_rect(fill = "grey67"))
  ))
  expect_true(ggcorrplot:::.panel_is_light(
    ggplot2::theme_minimal() +
      ggplot2::theme(panel.background = ggplot2::element_rect(fill = "grey70"))
  ))
})

test_that("resolving the background never errors, whatever ggtheme holds", {
  for (value in list(NULL, NA, 42, "nonsense", list(), ggplot2::element_blank(),
                     function() stop("boom"), function() 42)) {
    result <- ggcorrplot:::.panel_is_light(value)
    expect_true(is.logical(result) && length(result) == 1L && !is.na(result))
  }
})
