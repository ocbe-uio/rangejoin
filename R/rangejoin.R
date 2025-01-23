#' @title Range join
#' @description Performs a range join between case and control data.
#' @param data A list with two data frames: case and ctrl
#' @param max_ctrl_per_case Maximum number of controls per case.
#' @param age_range A vector with two elements: the minimum and maximum age
#' difference between cases and controls.
#' @return A list with two data frames: control_data and case.
#' @export
rangejoin <- function(data, max_ctrl_per_case, age_range) {
  # Extracting data
  case <- data$case
  ctrl <- data$ctrl

  # Validation
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
  message("Matching up to ", max_ctrl_per_case, " controls per case")
  for (cs in seq_len(nrow(case))) {
    progress_bar <- txtProgressBar(max = nrow(case), style = 2)
    for (ct in seq_len(nrow(ctrl))) {
      condition0 <- is.na(ctrl$case_match[ct])
      condition1 <- ctrl$sex[ct] == case$sex[cs]
      condition2 <- ctrl$age[ct] >= case$age[cs] + age_range[1]
      condition3 <- ctrl$age[ct] <= case$age[cs] + age_range[2]
      if (condition0 && condition1 && condition2 && condition3) {
        ctrl$case_match[ct] <- case$nnid[cs]
        case$n_controls[cs] <- case$n_controls[cs] + 1L
        if (case$n_controls[cs] == max_ctrl_per_case) {
          break
        }
      }
    }
    setTxtProgressBar(progress_bar, cs)
  }
  close(progress_bar)

  # Returning datasets
  list("ctrl" = ctrl, "case" = case)
}
