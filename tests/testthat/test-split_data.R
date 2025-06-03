test_that("splitting data works", {
  splat_1 <- split_data(demo_data)
  splat_2 <- split_data(demo_data, "case_control")
  splat_3 <- split_data(demo_data, "sex")
  expect_named(splat_1, c("case", "ctrl", "all"))
  expect_equal(
    nrow(splat_1[["ctrl"]]) / nrow(splat_1[["case"]]), 2, tolerance = 1
  )
  expect_named(splat_2, c("case", "ctrl", "all"))
  expect_equal(
    nrow(splat_2[["ctrl"]]) / nrow(splat_2[["case"]]), 2, tolerance = 1
  )
  expect_named(splat_3, c("case", "ctrl", "all"))
  expect_equal(
    nrow(splat_3[["ctrl"]]) / nrow(splat_3[["case"]]), 2, tolerance = 1
  )
  expect_equal(vapply(splat_1, length, 0), c("case" = 4, "ctrl" = 4, "all" = 4))
  expect_equal(vapply(splat_2, length, 0), c("case" = 3, "ctrl" = 3, "all" = 3))
  expect_equal(vapply(splat_3, length, 0), c("case" = 3, "ctrl" = 3, "all" = 3))
})
