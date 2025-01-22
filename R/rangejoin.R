#' @title Range join
#' @export
rangejoin <- function(ctrl_data, case_data, max_ctrl_per_case, age_range) {
    # Validation
    if (max_ctrl_per_case > nrow(ctrl_data) / nrow(case_data)) {
        max_ctrl_per_case <- floor(nrow(ctrl_data) / nrow(case_data))
        warning(
            "Not enough control cases. Setting max_ctrl_per_case to ",
            max_ctrl_per_case
        )
    }

    # Performing the range join
    message("Matching up to ", max_ctrl_per_case, " controls per case")
    for (case in seq_len(nrow(case_data))) {
    progress_bar <- txtProgressBar(style = 3)
        for (ctrl in seq_len(nrow(ctrl_data))) {
            condition0 <- is.na(ctrl_data$case_match[ctrl])
            condition1 <- ctrl_data$sex[ctrl] == case_data$sex[case]
            condition2 <- ctrl_data$age[ctrl] >= case_data$age[case] + age_range[1]
            condition3 <- ctrl_data$age[ctrl] <= case_data$age[case] + age_range[2]
            if (condition0 & condition1 & condition2 & condition3) {
                ctrl_data$case_match[ctrl] <- case_data$nnid[case]
                case_data$n_controls[case] <- case_data$n_controls[case] + 1L
                if (case_data$n_controls[case] == max_ctrl_per_case) {
                    break
                }
            }
        }
        setTxtProgressBar(progress_bar, case / nrow(case_data))
    }
    close(progress_bar)

    # Returning datasets
    return(list("control_data" = ctrl_data, "case_data" = case_data))
}
