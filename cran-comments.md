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
* win-builder release (R 4.x)
* win-builder devel (R-devel)

## R CMD check results
Local `R CMD check --as-cran` on the 0.3.0 tarball: 0 errors | 0 warnings |
0 notes. The same holds with `_R_CHECK_DEPENDS_ONLY_=true`. win-builder
(release and R-devel) reported no errors and no warnings.

Some runs on the local machine add an "unable to verify current time" NOTE when
the host cannot reach a time server; it is environmental and does not appear on
CI.

## Reverse dependencies
There are 22 reverse dependencies (15 Depends/Imports, 7 Suggests). The check was
scoped to the behavior-changing surface, which is the two intentional changes
listed above; everything else in this release is additive, behind new arguments
whose defaults reproduce the 0.2.0 behavior.

I read the source of all 22 and extracted every `ggcorrplot()` / `cor_pmat()`
call site:

* No reverse dependency passes `as.is`, so the axis-order change reaches none of
  them.
* Only two pass `show.diag`, both `TRUE`; the changed code path needs
  `show.diag = FALSE` on a non-square matrix, which no reverse dependency does.
* All calls bind only `corr` positionally and name every other argument. The nine
  new arguments are appended at the end of the signature, no argument was removed
  or reordered, and the defaults of all 35 pre-existing arguments are unchanged.

No new problems were found.
