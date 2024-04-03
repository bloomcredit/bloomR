#' Run decision model.
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


# check prerequisite data -------------------------------------------------
  credit_score_exists <- exists("credit_scores", where = credit_data)
  months_recent_delinquency_exists <- exists("attributes", where = credit_data)

  if (!credit_score_exists | !months_recent_delinquency_exists) {
    stop("Insufficient data provided. Missing `credit_data$credit_scores` and/or `credit_data$attributes`.")
  } else {
    approved <- ((min(credit_data$credit_scores$value) >= 600) &
                   (
                     as.numeric(credit_data$attributes$months_recent_delinquency) >= 24
                   ) &
                   (ifelse(
                     exists("tradeline_dti", credit_data$attributes),
                     (as.numeric(credit_data$attributes$tradeline_dti) <= 35),
                     TRUE
                   )))

    denied <- ((min(credit_data$credit_scores$value) < 600) &
                 (
                   as.numeric(credit_data$attributes$months_recent_delinquency) < 24
                 ) &
                 (ifelse(
                   exists("tradeline_dti", credit_data$attributes),
                   (as.numeric(credit_data$attributes$tradeline_dti) < 35),
                   TRUE
                 )))
  }



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
