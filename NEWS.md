# ggcorrplot 0.2.0.9000

## New features

- New argument `scale.square` to size the squares by the absolute correlation
  when `method = "square"`: `ggcorrplot(corr, scale.square = TRUE)` scales each
  square (larger = stronger correlation) on top of the fill color, the classic
  corrplot size-scaled square look. Defaults to `FALSE` (constant full-cell
  squares, unchanged); has no effect for `method = "circle"` (circles are always
  sized).

- New argument `preset` for publication-grade output in one token:
  `ggcorrplot(corr, preset = "publication")` sets white cell outlines and the
  colorblind-safe `"RdBu"` palette. It only fills arguments you did not supply, so
  anything you pass explicitly (e.g. `outline.color`, `colors`, `palette`)
  overrides the preset. Defaults to `NULL` (no preset), leaving existing calls
  unchanged.

- New argument `palette` to pick a built-in colorblind-safe diverging palette for
  the fill gradient: `ggcorrplot(corr, palette = "RdBu")` (or `"PuOr"`). Each is
  an 11-stop ramp with white at zero and the usual cool = negative / warm =
  positive polarity, so it is a one-word alternative to spelling out `colors`.
  Defaults to `NULL` (use `colors`), leaving existing calls unchanged.

- New value `insig = "stars"` to mark the significant cells with significance
  stars (`***`/`**`/`*` for p < 0.001/0.01/0.05) instead of crossing out the
  insignificant ones. Works without `lab`, so `ggcorrplot(corr, p.mat = p.mat,
  insig = "stars")` is a standalone significance map. The default `insig = "pch"`
  is unchanged.

- New arguments `lower.method` and `upper.method` for a mixed layout: a different
  glyph per triangle, e.g. the coefficients as numbers in the lower triangle,
  circles in the upper, and the variable names on the diagonal
  (`ggcorrplot(corr, lower.method = "number", upper.method = "circle")`). Each
  accepts "square", "circle" or "number"; both default to `NULL`, leaving the
  single-method output unchanged (#85). Requested by @Breeze-Hu.

- New argument `hc.rect` to draw rectangles around the clusters of a
  hierarchically-ordered correlogram: `ggcorrplot(corr, hc.order = TRUE,
  hc.rect = 3)` frames the 3 clusters obtained by cutting the tree. Defaults to
  `NULL` (no rectangles). The rectangle outline color is set with the companion
  argument `hc.rect.col` (default `"gray30"`, e.g. `hc.rect.col = "white"`).

## Main changes

- The plot axis is now always drawn in the matrix (row/column) order. This fixes
  a bug where `hc.order = TRUE` was silently ignored for correlation matrices with
  numeric-looking variable names (e.g. `"1"`, `"2"`, ...): `reshape2::melt`
  type-converted such names to a continuous axis sorted by value, discarding the
  clustering order (#37). As a consequence, the `as.is` argument no longer affects
  the plot (it is kept only for backward compatibility) -- a call using
  `as.is = TRUE`, which previously produced an alphabetically-ordered axis, now
  follows the matrix order like every other call. Reported by @cabaez (#37).

## Minor changes

- Added an introductory vignette, `vignette("ggcorrplot")`, walking through the
  package end to end: the inputs, glyphs and layouts, clustering, coefficient
  labels, significance styles, and theming.

- Added a second vignette,
  `vignette("publication-ready-correlation-plots")`, a gallery of finished
  publication-ready correlogram recipes (clustered, triangle, significance,
  circle, colorblind palette, edgeless heatmap, rectangular predictor-by-outcome
  matrix, and full ggplot2 polish).

- Internal refactor: the data-preparation pipeline and the glyph-layer
  construction are now factored into internal helpers, with no change to the
  output of any existing call. This is groundwork for forthcoming per-triangle
  layout options.

## Bug fixes

- `show.diag = FALSE` on a non-square matrix no longer blanks the wrong cells.
  The diagonal has no positional meaning for an m x n matrix, yet the removal
  wiped `min(m, n)` cells along the leading diagonal; it now removes only the
  genuine self-pairs (a row and column naming the same variable), leaving a
  matrix with disjoint row and column variables untouched. Square matrices, and
  non-square matrices without dimnames, are unaffected. Follows up the non-square
  support requested by @mt1022 (#5) and @qalid7 (#10).

# ggcorrplot 0.2.0

## New features

- New argument `sig.stars` to append significance stars (`***`, `**`, `*`) to
  the coefficient labels when `lab = TRUE` and a `p.mat` is supplied, e.g.
  `"-0.85**"`. Defaults to `FALSE` (#26, #41, #50; inspired by the ggcorrplot2
  package by @caijun).

- New argument `circle.scale` to scale the circle sizes when `method = "circle"`,
  useful when the output device size makes the default circles too small or too
  large. Defaults to `1` (contributed by @jdeut, #8).

- New argument `nsmall` to set a minimum number of decimals in the coefficient
  labels (e.g. `nsmall = 2` keeps trailing zeros such as 0.70). Defaults to `0`,
  the current behavior (#43; label-formatting idiom suggested by @PawelKulawiak in #15).

- New argument `legend.limit` to control the limits of the fill color scale.
  Defaults to `c(-1, 1)`; set `legend.limit = NULL` to use the data range, e.g.
  to display a covariance matrix (#54).

- The `colors` argument now accepts a vector of any length `>= 2`, not only 3.
  A length-3 vector still maps to low/mid/high via `scale_fill_gradient2` (default
  output unchanged); any other length is spread across the scale with
  `scale_fill_gradientn`, so an n-color palette such as
  `RColorBrewer::brewer.pal(11, "RdBu")` can be passed straight to `colors =`
  without adding a second fill scale (and without the "Scale for fill is already
  present" message) (#52). Requested by @glocke-senda.

- New argument `coord.fixed` (default `TRUE`) to optionally drop the fixed 1:1
  aspect ratio. Set `coord.fixed = FALSE` to let the cells fill the plotting
  area, which can look better with many long variable names (#40).

- New argument `lab_fontface` to set the font face (`"plain"`, `"bold"`,
  `"italic"`, `"bold.italic"`) of the correlation coefficient labels. Defaults
  to `"plain"`, the current behavior (#15).

- New argument `leading.zero` to drop the leading zero of the coefficient labels
  (e.g. `.23`, `-.67` instead of `0.23`, `-0.67`), common in correlation tables.
  Defaults to `TRUE` (leading zero kept, current behavior); set
  `leading.zero = FALSE` to remove it (#15; idiom from @PawelKulawiak's comment).

- New arguments `tl.vjust` and `tl.hjust` to control the vertical and horizontal
  justification of the x-axis text labels. Both default to `1`, the current
  behavior (#56).

- New argument `use` in `cor_pmat()` to align the p-value matrix's `NA` pattern
  with a correlation matrix. The default `"pairwise.complete.obs"` keeps the
  current behavior; `use = "everything"` sets a pair to `NA` as soon as either
  variable has a missing value, matching `cor()`'s default so the two matrices
  line up (@elizabethwe, #51).

## Minor changes

- Replaced the deprecated `ggplot2::aes_string()` with tidy-evaluation `aes()`
  internally, silencing the ggplot2 deprecation warnings on recent `ggplot2`
  versions. Default output is unchanged (#57, #58, #59, #60, #61). Based on the
  contribution by @jeherschberger (#62).

- Added a `CITATION` file so `citation("ggcorrplot")` returns a proper
  reference (#42, #47).

- Added an internal structural regression test suite that asserts on the built
  plot (layer composition, built data, fill-scale semantics, coordinate system,
  ordering and significance handling), so the plot's structure is checked on CI
  and CRAN and not only by the local visual snapshots (#81).

## Bug fixes

- `cor_pmat()` no longer aborts when a pair of variables has fewer than three
  overlapping non-missing observations to correlate (e.g. two variables that
  never co-occur). Such a pair now returns `NA` for that cell instead of erroring
  out for the whole matrix; pairs that can be tested are computed as before
  (@elizabethwe, #51).

- A non-square (m x n) correlation matrix now gives a clear error when combined
  with `hc.order = TRUE` or `type = "lower"`/`"upper"` (which require a square
  matrix), instead of silently producing an incorrect plot. `type = "full"`
  still works for non-square matrices (#5, #10).

- The significance markers no longer error or misalign when the correlation
  matrix and the p-value matrix have different missing-value patterns. P-values
  are now matched to each cell by name instead of by row position.

- When `hc.order = TRUE`, the hierarchical clustering is now computed on the
  unrounded correlation matrix. Previously the matrix was rounded to `digits`
  before clustering, so the internal rounding could introduce ties that changed
  the ordering (@buddha2490, #14).

- The `tl.col` argument (color of the axis text labels) is now applied; it was
  previously ignored. It defaults to `NULL`, inheriting the color from the theme,
  so the default appearance is unchanged (@LafontRapnouilTristan, #44, #45).

- The significance test is no longer affected by `hc.order`. Previously, when
  `hc.order = TRUE`, the p-value matrix was rounded to `digits` before being
  compared with `sig.level`, so a p-value just above the threshold (e.g. 0.054)
  could be shown as significant while the same data with `hc.order = FALSE`
  showed it as non-significant (@worden-lee, #25).

- The significance markers now stay aligned with the tiles when the matrix has
  numeric-looking names. The p-value matrix is now reshaped with the same
  `as.is` setting as the correlation matrix, so `as.is = TRUE` no longer places
  the markers off-plot (@cabaez, #37).

# ggcorrplot 0.1.4

## Minor changes

- New argument `as.is` added. A logical passed to melt.array. If TRUE, dimnames
  will be left as strings instead of being converted using type.convert
  (@fdetsch, [#24](https://github.com/kassambara/ggcorrplot/pull/24)).

- Gets rid of `NOTE` in CRAN daily checks about lazy data.

- Adds visual regression testing infrastructure using `vdiffr`.

- Removes warnings stemming from the latest version of `ggplot2`.

## Bug fixes

- The option `hc.method` is now taken into account (#mitchelfruin, #29)

- The option `show.diag` now works for full matrix (@arbet003 , #31)

# ggcorrplot 0.1.3
  
## New features
   
- Support an object of class `cor_mat` as returned by the function `cor_mat()`
  [rstatix package]

## Minor changes
   
Merging with pull request 16 (@IndrajeetPatil,
[#16](https://github.com/kassambara/ggcorrplot/pull/16)), which addresses the
following issues:

1. In all `README` and `roxygen` examples, the argument `outline.color` was
   written as `outline.col`, which created `warnings` in `RStudio` scripts about
   the partial matching of arguments. Fixed that.

2. Styled the code in `tidyverse` style guide (both in `R` script and `README`
   file).

3. Added spelling tests to make sure no spelling error fall through the cracks.

4. Bumped up the package version to highlight that this is the development
   version. Added a few more badges to `README` to convey the same thing.

5. The `digits` argument (introduced in #12) wasn't working properly
   (https://github.com/IndrajeetPatil/ggstatsplot/issues/93). This is now fixed.
   Also added an example to show that this works.

## Bug fixes
   
- When `insig = "blank"` correlation labels are no longer displayed for
  insignificant correlations (@axitamm,
  [#17](https://github.com/kassambara/ggcorrplot/pull/17))

# ggcorrplot 0.1.2
   
## Minor changes
   
- New argument `digits` added to `ggcorrplot()` (@IndrajeetPatil,
  [#12](https://github.com/kassambara/ggcorrplot/pull/12).

- New argument ggtheme added to `ggcorrplot()` (@IndrajeetPatil,
  [#11](https://github.com/kassambara/ggcorrplot/pull/11).

## Bug fixes
   
- Bug fix for label argument inside ggplot2::geom_text (@alekrutkowski,
  [#1](https://github.com/kassambara/ggcorrplot/pull/1))

- Now `ggcorrplot()` when both reshape and reshape2 packages are loaded
  ([#4](https://github.com/kassambara/ggcorrplot/issues/4))

# ggcorrplot 0.1.1

## New features
   
- ggcorrplot(): visualize a correlation matrix

- cor_pmat(): compute a correlation matrix p-values

