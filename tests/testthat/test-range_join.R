test_that("multiplication works", {
  expect_error(
    range_join(demo_data),
    "Data must be a list with at least two data frames: case and ctrl"
  )
  df <- split_data(demo_data, "case_control")
  expect_error(range_join(df), '"max_ctrl_per_case" is missing, with no defaul')
  expect_warning(
    range_join(df, max_ctrl_per_case = 2, join_var_range = c(-5, 5)),
    "Not enough control cases. Setting max_ctrl_per_case to 1"
  )
  rj <- range_join(df, max_ctrl_per_case = 1, join_var_range = c(-5, 5))
  expect_equal(max(rj$case$n_controls), 1L)
  cases_with_controls <- nrow(rj$case[rj$case$n_controls == 1L, ])
  controls_with_cases <- sum(!is.na(rj$ctrl$case_match))
  expect_equal(cases_with_controls, controls_with_cases)
})
