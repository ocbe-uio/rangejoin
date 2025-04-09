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

#' @title Print method for rangejoin_rangejoin
#' @description Prints the structure of the joined data.
#' @param x An object of class `rangejoin_rangejoin`.
#' @param ... Additional arguments (not used).
#' @export
print.rangejoin_rangejoin <- function(x, ...) {
  for (cc in c("case", "ctrl")) {
    message(cc)
    print(head(x[[cc]], n = 10L))
    obs <- nrow(x[[cc]])
    if (obs > 10L) {
      message("Plus ", obs - 10L, " more cases for a total of ", obs, " cases")
    }
  }
}
