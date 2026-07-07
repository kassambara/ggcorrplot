# ggcorrplot 0.1.4

## New features

- New argument `nsmall` to set a minimum number of decimals in the coefficient
  labels (e.g. `nsmall = 2` keeps trailing zeros such as 0.70). Defaults to `0`,
  the current behavior (#43; label-formatting idiom suggested by @PawelKulawiak in #15).

- New argument `legend.limit` to control the limits of the fill color scale.
  Defaults to `c(-1, 1)`; set `legend.limit = NULL` to use the data range, e.g.
  to display a covariance matrix (#54).

## Minor changes
  
- New argument `as.is` added. A logical passed to melt.array. If TRUE, dimnames
  will be left as strings instead of being converted using type.convert
  (@fdetsch, [#24](https://github.com/kassambara/ggcorrplot/pull/24)).

- Gets rid of `NOTE` in CRAN daily checks about lazy data.

- Adds visual regression testing infrastructure using `vdiffr`.

- Removes warnings stemming from the latest version of `ggplot2`.

- Replaced the deprecated `ggplot2::aes_string()` with tidy-evaluation `aes()`
  internally, silencing the ggplot2 deprecation warnings on recent `ggplot2`
  versions. Default output is unchanged (#57, #58, #59, #60, #61). Based on the
  contribution by @jeherschberger (#62).

- Added a `CITATION` file so `citation("ggcorrplot")` returns a proper
  reference (#42, #47).

## Bug fixes

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

