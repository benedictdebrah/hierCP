test_that("independent detection returns list", {
  sim <- sim_hier_data(Tn=60, nb=2)
  Y <- sim$Y
  lev <- sim$levels
  taus <- detect_cp_independent(Y, lev, w=10)
  expect_true(is.list(taus))
})
