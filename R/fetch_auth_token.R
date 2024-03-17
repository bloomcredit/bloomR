#' Fetch Oauth2 token to access the Bloom Credit API.
#'
#' @param audience `character` Must be one of: "dev" or "prod", corresponding to the Audience Parameter.
#' See more here: https://developers.bloomcredit.io/docs/environments-1
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
fetch_auth_token <- function(audience, client_id = getOption('bloom_api_client_id'), client_secret = getOption('bloom_api_client_secret')) {

  stopifnot(
    "`audience` parameter must be one of: 'dev' or 'prod'" = tolower(audience) %in% c("dev", "prod")
  )

# set audience and url based on `audience` parameter ----------------------
  url <- set_token_url(audience)

# set `client_id` and `client_secret` if not provided by user -------------
  if (is.null(client_id)) {
    cli::cli_alert_info("Make sure to paste your {.arg client_id} {.emph exactly as it appears} without quotations.")
    client_id <- readline(prompt = "Enter your client_id: ")
  }

  if (is.null(client_secret)){
    cli::cli_alert_info("Make sure to paste your {.arg client_secret} {.emph exactly as it appears} without quotations.")
    client_secret <- readline(prompt = "Enter your client_secret: ")
  }

# construct API call ------------------------------------------------------
  body <- list(
    client_id = client_id,
    client_secret = client_secret,
    audience = "dev-api",
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
