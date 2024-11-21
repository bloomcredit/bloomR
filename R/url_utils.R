#' Set the API URL based on audience.
#'
#' @param audience `character` Must be one of: "dev" or "prod", corresponding to the Audience Parameter.
#' See more here: https://developers.bloomcredit.io/docs/environments-1
#'
#' @examples
#' \dontrun{
#' url <- set_token_url("dev")
#' }
#'
#' @export
#'
#' @return `character` URL for Oauth2 token API endpoint.
set_token_url <- function(audience) {

  if (tolower(audience) == "dev") {
    url <- "https://authn.bloom.dev/oauth2/token"
  }

  if (tolower(audience) == "prod") {
    url <- "https://authn.bloomcredit.io/oauth2/token"
  }

  return(url)

}


#' Set the API URL based on audience to register a consumer.
#'
#' @param audience `character` Must be one of: "dev" or "prod", corresponding to the Audience Parameter.
#' See more here: https://developers.bloomcredit.io/docs/environments-1
#'
#'
#' @examples
#' \dontrun{
#' registration_url <- set_registration_url("dev")
#' }
#'
#' @export
#' @return `character` URL for consumer registration endpoint.
set_registration_url <- function(audience) {

  if (tolower(audience) == "dev") {
    url <- "https://sandbox.bloom.dev/v2/core/consumers"
  }

  if (tolower(audience) == "prod") {
    url <- "https://api.bloomcredit.io/v2/core/consumers"
  }

  return(url)

}


#' Set the API URL based on audience to order credit data.
#'
#' @param audience `character` Must be one of: "dev" or "prod", corresponding to the Audience Parameter.
#' See more here: https://developers.bloomcredit.io/docs/environments-1
#'
#' @examples
#' \dontrun{
#' order_credit_data_url <- set_order_credit_data_url("dev")
#' }
#'
#' @export
#' @return `character` URL for consumer registration endpoint.
set_order_credit_data_url <- function(audience) {

  if (tolower(audience) == "dev") {
    url <- "https://sandbox.bloom.dev/v2/data-access/orders/"
  }

  if (tolower(audience) == "prod") {
    url <- "https://api.bloomcredit.io/v2/data-access/orders/"
  }

  return(url)

}

#' Set the API URL based on audience to retrieve credit data report.
#'
#' @param audience `character` Must be one of: "dev" or "prod", corresponding to the Audience Parameter.
#' @param order_id `character` Order ID for credit report.
#'
#' @examples
#' \dontrun{
#' credit_data_url <- set_get_credit_data_url("dev", "abc123")
#' }
#'
#' @export
#' @return `character` URL for credit data report endpoint.
set_get_credit_data_url <- function(audience, order_id) {

  if (tolower(audience) == "dev") {
    url <- glue::glue("https://sandbox.bloom.dev/v2/data-access/orders/{order_id}/full-report")
  }

  if (tolower(audience) == "prod") {
    url <- glue::glue("https://api.bloomcredit.io/v2/data-access/orders/{order_id}/full-report")
  }

  return(url)

}
