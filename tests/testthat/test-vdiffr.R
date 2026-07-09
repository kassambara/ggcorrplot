if (getRversion() >= "4.1") {
  test_that("plots are rendered correctly", {
    skip_on_cran()
    # vdiffr snapshots are SVG-engine/ggplot2-version specific and are not
    # portable across CI runners; validate them locally, not on CI.
    skip_on_ci()
    skip_if_not_installed("vdiffr")

    # compute needed details
    set.seed(123)
    corr <- round(cor(mtcars), 1)
    p.mat <- cor_pmat(mtcars)

    set.seed(123)
    vdiffr::expect_doppelganger(
      title = "ggcorrplot works - default",
      fig = ggcorrplot(corr)
    )

    set.seed(123)
    vdiffr::expect_doppelganger(
      title = "ggcorrplot works - circle",
      fig = ggcorrplot(corr, method = "circle")
    )

    set.seed(123)
    vdiffr::expect_doppelganger(
      title = "ggcorrplot works - hc",
      fig = ggcorrplot(corr, hc.order = TRUE, outline.color = "white")
    )

    set.seed(123)
    vdiffr::expect_doppelganger(
      title = "ggcorrplot works - mixed",
      fig = ggcorrplot(corr,
        lower.method = "number", upper.method = "circle",
        show.legend = FALSE
      )
    )

    set.seed(123)
    vdiffr::expect_doppelganger(
      title = "ggcorrplot works - hc.rect",
      fig = ggcorrplot(corr, hc.order = TRUE, hc.rect = 3, outline.color = "white")
    )

    set.seed(123)
    vdiffr::expect_doppelganger(
      title = "ggcorrplot works - insig stars",
      fig = ggcorrplot(corr, p.mat = p.mat, insig = "stars")
    )

    set.seed(123)
    # non-square matrix with show.diag = FALSE: only the genuine self-pairs
    # (disp, hp, wt) are blanked, not the positional diagonal
    nonsq <- round(cor(mtcars), 1)[
      c("disp", "hp", "wt"), c("mpg", "cyl", "disp", "hp", "wt")
    ]
    vdiffr::expect_doppelganger(
      title = "ggcorrplot works - non-square no-diag",
      fig = ggcorrplot(nonsq, show.diag = FALSE, lab = TRUE)
    )

    set.seed(123)
    vdiffr::expect_doppelganger(
      title = "ggcorrplot works - palette RdBu",
      fig = ggcorrplot(corr, palette = "RdBu", outline.color = "white", lab = TRUE)
    )

    set.seed(123)
    vdiffr::expect_doppelganger(
      title = "ggcorrplot works - preset publication",
      fig = ggcorrplot(corr, preset = "publication", lab = TRUE)
    )

    set.seed(123)
    vdiffr::expect_doppelganger(
      title = "ggcorrplot works - scale square",
      fig = ggcorrplot(corr, scale.square = TRUE, outline.color = "white")
    )

    set.seed(123)
    vdiffr::expect_doppelganger(
      title = "ggcorrplot works - cell grid circle",
      fig = ggcorrplot(corr, method = "circle", cell.grid = TRUE)
    )
  })
}
