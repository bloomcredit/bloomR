#' Run decision model.
#' TODO: draft - in progress...
#'
#' @param credit_data `list` retned from [bloomR::get_credit_data].
#'
#' @return `character` decision from model based on `credit_data`.
#'
#' @examples
#' \dontrun{
#' get_decision(credit_data = credit_data)
#' }
#'
#' @export
get_decision <- function(credit_data) {
  approved <- (
    (min(credit_data$credit_scores$value) >= 600) &
      (as.numeric(credit_data$attributes$months_recent_delinquency) >= 24) &
      (ifelse(
        exists("tradeline_dti", credit_data$attributes),
        (as.numeric(credit_data$attributes$tradeline_dti) <= 35),
        TRUE
      ))
  )

  denied <- (
    (min(credit_data$credit_scores$value) < 600) &
      (as.numeric(credit_data$attributes$months_recent_delinquency) < 24) &
      (ifelse(
        exists("tradeline_dti", credit_data$attributes),
        (as.numeric(credit_data$attributes$tradeline_dti) < 35),
        TRUE
      ))
  )

  if (credit_data$ofac_statuses$reference != "") {
    print(
      paste0(
        "unable to process; ", credit_data$ofac_statuses$issue_source
      )
    )
  } else if (approved) {
    print("decision: approved")
  } else if (denied) {
    print("decision: denied")
  } else {
    print("decision: manual review required")
  }
}
