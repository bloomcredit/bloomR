#' Register a consumer.
#'
#' @param url `character` URL to endpoint.
#' @param consumer_info `list` containing the required named fields:
#' - `first_name`:
#' - `last_name`:
#' - `city`:
#' - `line1`:
#' - `state_code`:
#' - `zipcode`: (character) must be exactly 5 digits, e.g.: "90210".
#' - `address_primary`: (boolean) must be `TRUE` or `FALSE`.
#' @param auth_token `character` Oauth2 token returned by [bloomR::fetch_auth_token].
#'
#' @return `console message` Confirmation or error from API response.
#'
#' @examples
#'
#' \dontrun{
#' consumer_info <- list(
#'   ssn = "123456789",
#'   city = "Scranton",
#'   line1 = "1725 Slough Avenue",
#'   state_code = "PA",
#'   zipcode = "18503",
#'   date_of_birth = "1964-03-15",
#'   first_name = "Michael",
#'   last_name = "Scott",
#'   address_primary = TRUE
#'   )
#'
#' auth_token <- fetch_auth_token(
#'   audience = "dev",
#'   client_id = "your_client_id",
#'   client_secret = "your_client_secret"
#'   )
#'
#'  register_consumer(
#'   audience = "dev",
#'   consumer_info = consumer_info,
#'   auth_token = auth_token
#'  )
#'
#' }
#'
#' @export
register_consumer <- function(url = set_registration_url("dev"), consumer_info, auth_token) {

  stopifnot(
    "`url` cannot be null." = !is.null(url),
    "`consumer_info` must be a list." = !is.list(consumer_info)
  )

# create request body -----------------------------------------------------
  body <- make_registration_body(consumer_info = consumer_info)

# set registration url ----------------------------------------------------


# send request ------------------------------------------------------------
  response <- httr::POST(
    url,
    httr::add_headers("Authorization" = paste("Bearer", auth_token), "Content-Type" = "application/json"),
    body = body,
    encode = "json"
  )

  httr::stop_for_status(response)

  if (response$status_code == "201") {
    content <- httr::content(response)
    cli::cli_alert_success("Consumer registered with id {content$data$id}")
    return(content$data$id)
  } else {
    cli::cli_alert_warning("Error registering consumer. Returned status code: {response$status_code}")
  }

}
