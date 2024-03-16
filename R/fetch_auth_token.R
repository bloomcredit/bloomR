#' Fetch Oauth2 token to access the Bloom Credit API.
#'
#' @param audience `character` Must be one of: "dev" or "prod", corresponding to the Audience Parameter.
#' See more here: https://developers.bloomcredit.io/docs/environments-1
#'
#'
#' @examples
#' \dontrun{
#' auth_token <- fetch_auth_token(audience = "dev")
#' }
#'
#' @return `character` Oauth2 token used to access the Bloom Credit API.
#'
#' @export
fetch_auth_token <- function(audience) {

  stopifnot(
    "`audience` parameter must be one of: 'dev' or 'prod'" = tolower(audience) %in% c("dev", "prod")
  )

  if (tolower(audience) == "dev") {
    audience <- "dev-api"
    url <- "https://auth.bloom.dev/oauth/token"
  }

  if (tolower(audience) == "prod") {
    audience <- "api.bloomcredit.io"
    url <- "https://auth.bloomcredit.io/oauth/token"
  }

  cli::cli_alert_info("You will be prompted for your {.arg client_id} and {.arg client_secret}
                      Paste them in the console {.emph without} quotation marks.")
  client_id <- readline(prompt = "Enter your client_id: ")
  client_secret <- readline(prompt = "Enter your client_secret: ")

  body <- list(
    client_id = client_id,
    client_secret = client_secret,
    audience = "dev-api",
    grant_type = "client_credentials"
  )

  response <- httr::POST(
    url,
    body = body,
    encode = "form",
    httr::add_headers("Content-Type" = "application/x-www-form-urlencoded")
  )

  httr::stop_for_status(response)

  token <- httr::content(response)$access_token

}
