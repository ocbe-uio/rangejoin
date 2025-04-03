#' @title Split dataset into case and control
#' @param data Dataset
#' @param split_variable Binary variable to split the dataset
#' @description Splits a data frame into case and control groups based on a
#' binary variable. If the variable is not provided, it randomly assigns
#' "Control" and "Case" labels to the data on a 2:1 ratio.
#' @return A list with two data frames: case and ctrl
#' @export
#' @examples
#' split_data(demo_data)
split_data <- function(data, split_variable) {
  if (missing(split_variable)) {
    split_variable <- "group"
    # Randomly assign 2 control subjects for each case
    data[[split_variable]] <- sample(
      c("Control", "Case"), nrow(data),
      replace = TRUE, prob = 2:1
    )
  }
  data[[split_variable]] <- factor(data[[split_variable]])

  # Validation
  split_levels <- levels(factor(data[[split_variable]]))

  ## Making sure split variable is binary
  n_levels <- length(split_levels)
  stopifnot("split_variable is not binary" = n_levels == 2)

  ## Check if split variable is already {"Control", "Case"}
  if (!all(split_levels %in% c("Control", "Case"))) {
    # Transforming split variable into factor
    data[[split_variable]] <- factor(
      data[[split_variable]],
      labels = c("Control", "Case"),
      ordered = FALSE
    )
  }

  # Returning split data
  data <- list(
    "case" = data[data[[split_variable]] == "Case", ],
    "ctrl" = data[data[[split_variable]] == "Control", ],
    "all" = data
  )
  class(data) <- c("rangejoin_splitdata", "list")
  data
}
