test_that("gm11 basic works", {
  x <- c(5,6,7,8,9)
  fit <- gm11_fit(x)
  fc  <- gm11_forecast(fit, h=2, n_obs=length(x))
  expect_length(fc, 2)
})
