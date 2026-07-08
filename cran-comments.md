## Submission

This is a feature and bug-fix update (0.2.0), following the current CRAN version
0.1.4.1. It adds several new, backward-compatible arguments to `ggcorrplot()`
(`sig.stars`, `circle.scale`, `nsmall`, `legend.limit`, `coord.fixed`,
`lab_fontface`, `leading.zero`, `tl.vjust`/`tl.hjust`), lets `colors` accept a
palette of any length, adds a `use` argument to `cor_pmat()`, and fixes a number
of defects (see NEWS.md). Default output for existing inputs is unchanged.

## Test environments
* local macOS, R 4.5.x
* GitHub Actions: macOS-release, Windows-release, Ubuntu-{devel, release, oldrel-1}
* win-builder release (R 4.x): OK
* win-builder devel (R-devel): OK

## R CMD check results
Local `R CMD check --as-cran`: 0 errors | 0 warnings | 0 notes.

The only local note is "unable to verify current time" (the check host cannot
reach a time server); it is environmental and does not appear on CI.

## Reverse dependencies
Checked all 22 reverse dependencies with a source-level scan scoped to the
behavior-changing surface of this release (the `hc.order` clustering/significance
fixes, `tl.col` now being applied, and `cor_pmat()` returning `NA` instead of
erroring for uncomputable pairs). No reverse dependency pins `ggcorrplot()` or
`cor_pmat()` output in a test, snapshot, or `expect_error()`, and every caller
supplies a square correlation matrix and default-compatible arguments, so no new
problems are introduced. All other changes are additive: new behavior is behind
new arguments whose defaults reproduce the current behavior, and default output
for existing inputs is unchanged.
