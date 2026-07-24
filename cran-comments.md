## Submission

This is a feature and bug-fix update (0.3.0), following the current CRAN version
0.2.0. It adds backward-compatible arguments to `ggcorrplot()` for a mixed
per-triangle layout (`lower.method`/`upper.method`), cluster rectangles
(`hc.rect`, `hc.rect.col`), built-in colorblind-safe palettes (`palette`), a
one-token publication preset (`preset`), size-scaled squares (`scale.square`) and
boxed cells (`cell.grid`, `cell.grid.col`), plus a new `insig = "stars"` value for
a standalone significance map. Two vignettes are added.

`Imports` gains `grDevices` (a base R package, used for `col2rgb()`/`rgb()`).

Two changes are intentional and alter output for a small set of inputs; both are
called out in NEWS.md:

* The plot axis is now always drawn in matrix (row/column) order. This fixes
  `hc.order = TRUE` being silently ignored for matrices with numeric-looking
  variable names; as a consequence `as.is` no longer affects the plot and is kept
  only for backward compatibility.
* `show.diag = FALSE` on a non-square matrix now removes only the genuine
  self-pairs rather than the positional diagonal.

Every other pre-existing argument combination produces byte-identical output to
0.2.0.

This follows 0.2.0 closer than usual because the two changes above correct cases
where the plot was silently wrong rather than erroring: `hc.order = TRUE` was
ignored for matrices with numeric-looking variable names, and `show.diag = FALSE`
blanked the wrong cells of a non-square matrix. I am happy to hold the release if
you would prefer the usual interval.

## Test environments
* local macOS, R 4.5.x
* GitHub Actions: macOS-release, Windows-release, Ubuntu-{devel, release, oldrel-1}
* win-builder release (R 4.x): TO RUN BEFORE SUBMISSION
* win-builder devel (R-devel): TO RUN BEFORE SUBMISSION

## R CMD check results
Local `R CMD check --as-cran` on the 0.3.0 tarball: 0 errors | 0 warnings |
1 note. The same holds with `_R_CHECK_DEPENDS_ONLY_=true`.

The note is environmental:

* "unable to verify current time" — the check host cannot reach a time server; it
  does not appear on CI.

## Reverse dependencies
TO RUN BEFORE SUBMISSION. The behavior-changing surface to scope the check to is
the two intentional changes listed above (matrix-order axis / inert `as.is`, and
non-square `show.diag = FALSE`); everything else in this release is additive,
behind new arguments whose defaults reproduce the 0.2.0 behavior.
