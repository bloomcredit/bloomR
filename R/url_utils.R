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
#' @keywords internal
#'
#' @return `character` URL for Oauth2 token API endpoint.
set_token_url <- function(audience) {

  if (tolower(audience) == "dev") {
    url <- "https://auth.bloom.dev/oauth/token"
  }

  if (tolower(audience) == "prod") {
    url <- "https://auth.bloomcredit.io/oauth/token"
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
#' @keywords internal
#'
#' @return `character` URL for consumer registration endpoint.
set_registration_url <- function(audience) {

  if (tolower(audience) == "dev") {
    url <- "https://sandbox.bloom.dev/v2/core/consumers"
  }

  if (tolower(audience) == "prod") {
    url <- "https://auth.bloom.dev/v2/core/consumers"
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
#' @keywords internal
#'
#' @return `character` URL for consumer registration endpoint.
set_order_credit_data_url <- function(audience) {

  if (tolower(audience) == "dev") {
    url <- "https://sandbox.bloom.dev/v2/data-access/orders/"
  }

  if (tolower(audience) == "prod") {
    url <- "https://auth.bloom.dev/v2/data-access/orders/"
  }

  return(url)

}
