#' Fetch Oauth2 token to access the Bloom Credit API.
#'
#' @param url `character` URL to endpoint.
#' @param client_id `character`
#' @param client_secret `character`
#'
#' @examples
#' \dontrun{
#' auth_token <- fetch_auth_token(audience = "dev")
#' }
#'
#' @return `character` Oauth2 token used to access the Bloom Credit API.
#'
#' @export
fetch_auth_token <- function(url = set_token_url("dev"), client_id = getOption('bloom_api_client_id'), client_secret = getOption('bloom_api_client_secret')) {

  stopifnot(
    "`url` cannot be null." = !is.null(url)
  )

# set `client_id` and `client_secret` if not provided by user -------------
  if (is.null(client_id)) {
    cli::cli_alert_info("Make sure to paste your {.arg client_id} {.emph exactly as it appears} without quotations.")
    client_id <- readline(prompt = "Enter your client_id: ")
  }

  if (is.null(client_secret)){
    cli::cli_alert_info("Make sure to paste your {.arg client_secret} {.emph exactly as it appears} without quotations.")
    client_secret <- readline(prompt = "Enter your client_secret: ")
  }

  auth_audience <- "dev-api"
  if (tolower(audience) == "prod") {
    auth_audience <- "api.bloomcredit.io"
  }

# construct API call ------------------------------------------------------
  body <- list(
    client_id = client_id,
    client_secret = client_secret,
    audience = auth_audience,
    scope = "data-access:all",
    grant_type = "client_credentials"
  )

  headers <- httr::add_headers(
    "Content-Type" = "application/x-www-form-urlencoded"
    )

# make API call -----------------------------------------------------------
  response <- httr::POST(
    url,
    body = body,
    encode = "form",
    headers
  )

# handle API call errors --------------------------------------------------
  httr::stop_for_status(response)


# parse access token ------------------------------------------------------
  token <- httr::content(response)$access_token

  return(token)

}
