#' @title Print method for rangejoin_splitdata
#' @description Prints the structure of the original data, case data, and
#'  control data.
#' @param x An object of class `rangejoin_splitdata`.
#' @param ... Additional arguments (not used).
#' @export
print.rangejoin_splitdata <- function(x, ...) {
  message("Original data structure")
  str(x[["all"]])
  message("Case data structure")
  str(x[["case"]])
  message("Control data structure")
  str(x[["ctrl"]])
}
