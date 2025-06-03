#' @title Summary Method for Range Join Objects
#' @description Summarize a range join object to show relevant statistics.
#' @param object An object of class \code{rangejoin_rangejoin} created by the
#'   \code{rangejoin} function.
#' @param ... Additional arguments (not used).
#' @author Waldir Leoncio
#' @export
#' @examples
#' set.seed(3)
#' dt <- split_data(demo_data)
#' rj <- range_join(dt, max_ctrl_per_case = 2L, age_range = c(-3, 3))
#' summary(rj)
summary.rangejoin_rangejoin <- function(object, ...) {
  message("Number of controls matched per case")
  print(table(object[["ctrl"]][["case_match"]], useNA = "ifany"))
  message("\nFrequency of control matches")
  print(table(object[["case"]][["n_controls"]], useNA = "ifany"))
}
