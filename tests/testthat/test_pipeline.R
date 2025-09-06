test_that("pipeline runs end-to-end", {
  sim <- sim_hier_data(Tn=80, nb=2)
  Y <- sim$Y; S <- sim$S; lev <- sim$levels
  h <- 6
  hier <- new_hierarchy(S=S, levels=lev, series_names=colnames(Y))
  res <- fit_forecast(Y, hier, h=h, strategy="topdown")
  expect_true(is.list(res))
  expect_true(all(dim(res$yhatB) == c(h, ncol(S))))
})
