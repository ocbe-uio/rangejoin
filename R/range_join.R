#' @title Range join
#' @description Performs a range join between case and control data.
#' @param data A list with two data frames: case and ctrl
#' @param max_ctrl_per_case Maximum number of controls per case.
#' @param join_var The variable to join on. Default is "age".
#' @param join_var_range A vector with two elements: the minimum and maximum age
#' @param quiet Logical. If TRUE, suppresses progress messages.
#' difference between cases and controls.
#' @return A list with two data frames: control_data and case.
#' @examples
#' set.seed(3)
#' dt <- split_data(demo_data)
#' range_join(dt, max_ctrl_per_case = 2L, join_var_range = c(-3, 3))
#' @export
range_join <- function(
    data, max_ctrl_per_case, join_var_range, join_var = "age", quiet = TRUE) {
  # Validating data structure
  if (!is.list(data) || !(all(c("case", "ctrl") %in% names(data)))) {
    stop("
      Data must be a list with at least two data frames: case and ctrl.
      You may want to run split_data() to create a suitable list.
    ")
  }

  # Extracting data
  case <- data[["case"]]
  ctrl <- data[["ctrl"]]

  # Validating sizes of data frames
  stopifnot("Can't have more cases than controls" = nrow(ctrl) >= nrow(case))
  ctrl_per_case <- nrow(ctrl) / nrow(case)
  if (max_ctrl_per_case > ctrl_per_case) {
    max_ctrl_per_case <- floor(ctrl_per_case)
    warning(
      "Not enough control cases. Setting max_ctrl_per_case to ",
      max_ctrl_per_case
    )
  }

  # Shuffling control data
  ctrl <- ctrl[sample(nrow(ctrl)), ]

  # Adding variables to control matches
  ctrl[["case_match"]] <- NA_integer_
  case[["n_controls"]] <- 0L

  # Performing the range join
  if (!quiet) {
    message("Matching up to ", max_ctrl_per_case, " controls per case")
  }
  for (cs in seq_len(nrow(case))) {
    if (!quiet) progress_bar <- txtProgressBar(max = nrow(case), style = 2)
    for (ct in seq_len(nrow(ctrl))) {
      if (ctrl_matches_case(ctrl, case, cs, ct, join_var, join_var_range)) {
        ctrl[["case_match"]][ct] <- as.integer(row.names(case[cs, ]))
        case[["n_controls"]][cs] <- case[["n_controls"]][cs] + 1L
        if (case[["n_controls"]][cs] == max_ctrl_per_case) {
          break
        }
      }
    }
    if (!quiet) setTxtProgressBar(progress_bar, cs)
  }
  if (!quiet) close(progress_bar)

  # Returning datasets
  data <- list("ctrl" = ctrl, "case" = case)
  class(data) <- c("rangejoin_rangejoin", "list")
  data
}

# Define a function to check the conditions
ctrl_matches_case <- function(ctrl, case, cs, ct, join_var, join_var_range) {
  is.na(ctrl[["case_match"]][ct]) &&
    ctrl[["sex"]][ct] == case[["sex"]][cs] &&
    ctrl[[join_var]][ct] >= case[[join_var]][cs] + join_var_range[1] &&
    ctrl[[join_var]][ct] <= case[[join_var]][cs] + join_var_range[2]
}
