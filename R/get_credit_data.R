#' Get credit report data based on `order_id`.
#'
#' @param url `character` URL to endpoint.
#' @param order_id `character` Order ID returned by [bloomR::order_credit_data].
#' @param auth_token `character` Oauth2 token returned by [bloomR::fetch_auth_token].
#'
#' @return `list` of data.frames containing credit data report information.
#'
#' @examples
#' \dontrun{
#' credit_data <- get_credit_data(
#'   audience = "dev",
#'   order_id = "your_order_id",
#'   auth_token = "your_auth_token"
#' )
#' }
#' @export
get_credit_data <- function(url = NULL, order_id, auth_token) {


# construct body ----------------------------------------------------------
  body <- paste0('{
  "query": "query FullOrderReport($ORDER_ID: uuid!, $order_id: String!) { credit_data_order: credit_data_order_by_pk(order_id: $ORDER_ID) { order_date order_id order_sku_id bankruptcies { current_disposition current_disposition_date date_filed date_reported disposition_date id industry_id order_id industry { category id name } } collections { account { account_number creditor { industry { category id name } address bureau email id industry_id name phone website } creditor_id date_closed date_opened id name type type_description } account_id credit_data_id current_balance custumer_number disputed id industry_code original_amount } consumer_id credit_scores { credit_data_id id model score_reasons { id narrative score_id } value } inquiries { credit_data_id date id industry_id inquirer_name industry { category id name } } mla_statuses { id order_id referral_contact_number regulated_identifier exist covered_borrower_status } ofac_statuses { reference order_id issue_source id exist } tradelines { account { account_number creditor { industry { category id name } address bureau email id industry_id name phone website } creditor_id date_closed date_opened id name type type_description } account_id account_rating credit_data_id credit_limit current_balance date_effective delinquency_date delinquency_earliest high_credit is_active late_30_days_total late_60_days_total late_90_days_total max_delinquency months_reviewed_count most_recent_payment_amount most_recent_payment_date past_due payment_pattern_start_date scheduled_monthly_payment payment_snapshots { id occurrence_date payment_status tradeline_id } } } tradeline_attributes: get_tradeline_attributes_by_order_id(args: {id: $order_id}) { total_tradelines open_tradelines tradeline_open_last_3_months tradeline_open_last_6_months tradeline_open_last_9_months tradeline_open_last_12_months months_oldest_tradeline_opened months_recent_tradeline_opened total_payment_obligation_open_tradelines months_recent_delinquency total_outstanding_balance_open_tradelines order_id } revolving_attributes: get_revolving_attributes_by_order_id(args: {id: $order_id}) { active_revolving_open_ended_tradelines_balance_opened_last_12_m active_revolving_open_ended_tradelines_balance_opened_last_24_m average_revolving_open_ended_tradeline_balance maximum_bank_card_utilization months_oldest_revolving_tradeline_opened months_recent_revolving_tradeline_opened months_recent_tradeline_revolving_delinquency open_revolving_tradelines revolving_rate revolving_tradelines_opened_last_6_months total_outstanding_balance_open_revolving_tradelines total_revolving_tradelines order_id } mortgage_attributes: get_mortgage_attributes_by_order_id(args: {id: $order_id}) { active_mortgage_tradelines_balance months_oldest_mortgage_tradeline_opened months_recent_mortgage_tradeline_delinquency months_recent_mortgage_tradeline_opened mortgage_tradelines_opened_last_6_months open_mortgage_tradelines outstanding_balance_open_mortgage_tradeline total_monthly_payment_obligation_for_open_mortgage_tradelines total_mortgage_tradelines order_id } auto_attributes: get_auto_attributes_by_order_id(args: {id: $order_id}) { auto_tradelines_opened_last_6_months months_oldest_auto_tradeline_opened months_recent_auto_tradeline_delinquency months_recent_auto_tradeline_opened open_autotradelines outstanding_balance_open_auto_tradeline total_auto_tradelines total_monthly_payment_obligation_for_open_auto_tradelines order_id } unsecured_installment_attributes: get_unsecured_installment_attributes_by_order_id(args: {id: $order_id}) { active_installment_tradelines active_installment_tradelines_opened_last_3_months months_oldest_unsecured_installment_tradelines_opened months_recent_unsecured_installment_tradeline_delinquency months_recent_unsecured_installment_tradelines_opened open_unsecured_installment_tradelines outstanding_balance_open_unsecured_installment_tradelines total_monthly_payment_obligation_for_open_unsecured_installment total_unsecured_installment_tradelines unsecured_installment_tradelines_opened_6_months order_id } student_loans_attributes: get_student_loans_attributes_by_order_id(args: {id: $order_id}) { months_oldest_student_loan_tradelines_opened months_recent_student_loan_tradelines_delinquency months_recent_student_loan_tradelines_opened open_student_loan_tradelines outstanding_balance_open_student_loan_tradelines student_loan_monthly_payment student_loan_tradelines_opened_last_6_months student_loans_deferment total_student_loan_tradelines order_id } delinquency_attributes: get_delinquency_attributes_by_order_id(args: {id: $order_id}) { tradelines_currently_dq30 tradelines_currently_dq60 order_id } specialized_attributes: get_specialized_attributes_by_order_id(args: {id: $order_id}) { bankruptcies foreclosures inquiries_last_6_months inquiries_on_file months_recent_bankruptcy months_recent_public_record months_recent_third_party_collection mortgage_trade_highest_high_credit non_medical_collections non_mortgage_non_inquiries_last_6_months percent_high_revolving_trade_utilization percent_opened_trades_last_24_months percent_satisfactory_trades_last_24_months tradeline_dti tradeline_ndi order_id } worst_attributes: get_worst_attributes_by_order_id(args: {id: $order_id}) { worst_rating_all_auto_tradelines_last_12_months worst_rating_all_mortgage_tradelines_last_12_months worst_rating_all_revolving_tradelines_last_12_months worst_rating_all_student_loan_tradelines_last_12_months worst_rating_all_unsecured_tradelines_last_12_months worst_rating_all_tradelines_last_12_months order_id } }",
  "variables": { "ORDER_ID": "', order_id, '"order_id": "', order_id, '" }
  }')

# construct headers -------------------------------------------------------
  headers <- c("accept" = "application/json", "Authorization" = paste("Bearer", auth_token))

# send GET request --------------------------------------------------------
  response <- httr::GET(
    url,
    body = body,
    httr::add_headers(headers),
    encode = "json"
    )

# parse report ------------------------------------------------------------

  if (tolower(httr::http_status(response)$category) == "success") {
    report <- httr::content(response, as = "parsed")
    browser()
    data <- parse_all_content(report)
    return(data)
  } else {
    cli::cli_alert_danger("Error fetching data - http response returned message: `{httr::http_status(response)$message}`")
  }

}

parse_tradelines <- function(tradelines) {

  tradelines %>%
    purrr::map(function(x) {
      creditor <- dplyr::bind_rows(x$account$creditor)
      creditor_data <- dplyr::bind_rows(x$account[names(x$account) != "creditor"])
      left_join(creditor, creditor_data, by = c("id" = "creditor_id"))
    }) %>%
    dplyr::bind_rows()

}

parse_ofac_statuses <- function(ofac_statuses) {
  ofac_statuses %>%
    purrr::map( ~ dplyr::bind_rows(.x)) %>%
    purrr::list_rbind()
}

parse_mla_statuses <- function(mla_statuses) {
  mla_statuses %>%
    purrr::map(function(x) {
      dplyr::bind_rows(x)
    }) %>%
    purrr::list_rbind()
}

parse_credit_scores <- function(credit_scores) {

  credit_scores %>%
    purrr::imap_dfr(function(data, name) {

      left_join(
        dplyr::bind_rows(data$score_reasons),
        dplyr::tibble(
          model = data$model,
          value = data$value,
          id = data$id,
          credit_data_id = data$credit_data_id
        ),
        by = c("score_id" = "id")
      )
    })

}

parse_credit_data_order <- function(credit_data_order) {

  dplyr::bind_rows(
    order_date = credit_data_order$order_date,
    order_id = credit_data_order$order_id,
    order_sku_id = credit_data_order$order_sku_id,
    consumer_id = credit_data_order$consumer_id
  )

}


parse_all_content <- function(report) {
  # parse all attributes
  attributes <- report[!names(report) %in% "credit_data_order"] %>%
    purrr::map(function(x) {
      dplyr::bind_rows(x)
    }) %>%
    purrr::reduce(left_join, by = "order_id") %>%
    select(
      "order_id",
      everything()
    )

  # credit_data_order
  credit_data_order <- report$credit_data_order

  credit_data <- parse_credit_data_order(credit_data_order)
  credit_scores <- parse_credit_scores(credit_data_order$credit_scores)
  mla_statuses <- parse_mla_statuses(credit_data_order$mla_statuses)
  ofac_statuses <- parse_ofac_statuses(credit_data_order$ofac_statuses)
  tradelines <- parse_tradelines(credit_data_order$tradelines)

  all_data <- list(
    attributes = attributes,
    credit_data = credit_data,
    credit_scores = credit_scores,
    mla_statuses = mla_statuses,
    ofac_statuses = ofac_statuses,
    tradelines = tradelines
  )

  return(all_data)
}
