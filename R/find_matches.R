#' @title Find Matches in Joined Data
#' @description Prints the matched controls for each case in the joined data.
#' @param joined_data The output of the `range_join` function
#' @param case_list A vector of case IDs to find matches for. If not provided,
#' it defaults to all cases in the joined data.
#' @author Waldir Leoncio
#' @export
#' @examples
#' set.seed(3)
#' dt <- split_data(demo_data)
#' rj <- range_join(dt, max_ctrl_per_case = 2L, join_var_range = c(-3, 3))
#' find_matches(rj, case_list = c(2, 16, 90))
find_matches <- function(
    joined_data, case_list = rownames(joined_data[["case"]])) {
  all_selected <- identical(case_list, rownames(joined_data[["case"]]))
  for (kz in as.character(case_list)) {
    if (!(kz %in% rownames(joined_data[["case"]]))) {
      if (!all_selected) {
        warning("Case ID ", kz, " not found in the case data.")
      }
      next
    }
    # Get the current case
    current_case <- joined_data[["case"]][kz, ]

    if (current_case[["n_controls"]] > 0L) {
      # Find matches in the control data
      matched_controls <- joined_data[["ctrl"]][
        joined_data[["ctrl"]][["case_match"]] %in% kz,
      ]

      # Print the matched controls
      message("Case ID: ", kz)
      print(current_case)
      message("Matched Controls:")
      print(matched_controls)
      cat("\n")
    }
  }
}
