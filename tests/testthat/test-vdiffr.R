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
  })
}
