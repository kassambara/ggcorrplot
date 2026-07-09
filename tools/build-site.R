#!/usr/bin/env Rscript
# Build the pkgdown site, then drop pages pkgdown generated from INTERNAL
# root markdown files.
#
# Why this wrapper exists: pkgdown renders every root-level `*.md` into a page
# (everything except README/LICENSE/NEWS and a few hardcoded templates) and it
# does NOT consult `.Rbuildignore`. So `CLAUDE.md` -- the internal project
# instructions -- would otherwise be published as `docs/CLAUDE.html`. pkgdown
# offers no option to exclude a root `.md`, so we post-process: any root `*.md`
# that is `.Rbuildignore`d is treated as internal and its built page removed.
# Run this (from the package root) instead of a bare `pkgdown::build_site()`:
#   Rscript tools/build-site.R

pkgdown::build_site(preview = FALSE)

root_md <- list.files(".", pattern = "\\.md$")
ignore <- readLines(".Rbuildignore", warn = FALSE)
ignore <- ignore[nzchar(ignore)]
is_internal <- function(f) any(vapply(ignore, function(p) grepl(p, f, perl = TRUE), logical(1)))
internal_md <- Filter(is_internal, root_md)

html <- file.path("docs", sub("\\.md$", ".html", internal_md))
drop <- html[file.exists(html)]
if (length(drop)) {
  unlink(drop)
  message("Removed internal pkgdown page(s): ", paste(basename(drop), collapse = ", "))
} else {
  message("No internal pkgdown pages to remove.")
}
