test_that("splitting data works", {
  splat <- split_data(example)
  expect_named(splat, c("case", "ctrl"))
  expect_equal(nrow(splat$ctrl) / nrow(splat$case), 2, tolerance = 1)
})
