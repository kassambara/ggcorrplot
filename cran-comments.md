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
* win-builder (devel and release) — to run before submission

## R CMD check results
Local `R CMD check --as-cran`: 0 errors | 0 warnings | 0 notes.

The only local note is "unable to verify current time" (the check host cannot
reach a time server); it is environmental and does not appear on CI.

## Reverse dependencies
This release is additive: all new behavior is behind new arguments that default
to the current behavior, and default output for existing inputs is byte-identical
(covered by the test suite). A scoped reverse-dependency check is to be run before
submission and its result recorded here.
