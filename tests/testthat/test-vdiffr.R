if (getRversion() >= "4.1") {
  test_that("plots are rendered correctly", {
    skip_on_cran()
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
  })
}
