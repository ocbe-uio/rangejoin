#' @title Range join
#' @description Performs a range join between case and control data.
#' @param data A list with two data frames: case and ctrl
#' @param max_ctrl_per_case Maximum number of controls per case.
#' @param age_range A vector with two elements: the minimum and maximum age
#' @param quiet Logical. If TRUE, suppresses progress messages.
#' difference between cases and controls.
#' @return A list with two data frames: control_data and case.
#' @export
range_join <- function(data, max_ctrl_per_case, age_range, quiet = TRUE) {
  # Extracting data
  case <- data$case
  ctrl <- data$ctrl

  # Validation
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
  ctrl$case_match <- NA_integer_
  case$n_controls <- 0L

  # Performing the range join
  if (!quiet) {
    message("Matching up to ", max_ctrl_per_case, " controls per case")
  }
  for (cs in seq_len(nrow(case))) {
    if (!quiet) progress_bar <- txtProgressBar(max = nrow(case), style = 2)
    for (ct in seq_len(nrow(ctrl))) {
      if (ctrl_matches_case(ctrl, case, cs, ct, age_range)) {
        ctrl$case_match[ct] <- as.integer(row.names(case[cs, ]))
        case$n_controls[cs] <- case$n_controls[cs] + 1L
        if (case$n_controls[cs] == max_ctrl_per_case) {
          break
        }
      }
    }
    if (!quiet) setTxtProgressBar(progress_bar, cs)
  }
  if (!quiet) close(progress_bar)

  # Returning datasets
  list("ctrl" = ctrl, "case" = case)
}

# Define a function to check the conditions
ctrl_matches_case <- function(ctrl, case, cs, ct, age_range) {
  is.na(ctrl$case_match[ct]) &&
    ctrl$sex[ct] == case$sex[cs] &&
    ctrl$age[ct] >= case$age[cs] + age_range[1] &&
    ctrl$age[ct] <= case$age[cs] + age_range[2]
}
